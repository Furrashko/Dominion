
package dominion.model.table

import dominion.model.*
import dominion.model.actioncard.{Market, Smithy, Village}

final class PlayTable(
                       initialPiles: List[(Card, Int)]
                     ) {
  private val piles: Map[String, CardPile] =
    initialPiles.map { case (card, amount) =>
      card.name -> new CardPile(card, amount)
    }.toMap

  require(
    piles.size == initialPiles.size,
    "Card pile names must be unique"
  )

  def allPiles: List[CardPile] =
    piles.values.toList.sortBy(_.card.name)

  def getPile(name: String): Option[CardPile] =
    piles.get(name)

  def availableCards: List[Card] =
    allPiles.filterNot(_.isEmpty).map(_.card)

  def buyCard(name: String): Option[Card] =
    piles.get(name).flatMap(_.take())

  def isPileEmpty(name: String): Boolean =
    piles.get(name).forall(_.isEmpty)

  def emptyPileCount: Int =
    piles.values.count(_.isEmpty)

  def isGameOver: Boolean =
    isPileEmpty("Province") || emptyPileCount >= 3

  private[model] def snapshot: Map[String, Int] =
    piles.view.mapValues(_.count).toMap

  private[model] def restore(
                              amounts: Map[String, Int]
                            ): Unit = {
    require(
      amounts.keySet == piles.keySet,
      "Snapshot does not match the current play table"
    )

    amounts.foreach { case (name, amount) =>
      piles(name).restore(amount)
    }
  }
}

object PlayTable {

  def standard(playerCount: Int): PlayTable = {
    require(
      playerCount >= 2 && playerCount <= 8,
      "Player count must be between 2 and 8"
    )

    val victoryCards =
      if (playerCount <= 2) 8 else 12

    val kingdomCards = List(
      (new Village, 10),
      (new Smithy, 10),
      (new Market, 10)
    )

    val basicCards = List(
      (CardFactory.createCopper(), 60 - 7 * playerCount),
      (CardFactory.createSilver(), 40),
      (CardFactory.createGold(), 30),
      (CardFactory.createEstate(), victoryCards),
      (CardFactory.createDuchy(), victoryCards),
      (CardFactory.createProvince(), victoryCards)
    )

    new PlayTable(basicCards ++ kingdomCards)
  }
}
