package dominion.model.table

import dominion.model.*
import dominion.model.playset.PlaySetDefinition

object PlayTableFactory {

  def create(
              playerCount: Int,
              definition: PlaySetDefinition
            ): PlayTable = {
    require(playerCount >= 2 && playerCount <= 8,
      "Player count must be between 2 and 8")

    // PlaySetDefinition already validates count and uniqueness of the cards.
    val actionCards = definition.createActionCards

    val victoryCards = if (playerCount <= 2) 8 else 12

    val basicCards = List(
      (CardFactory.createCopper(), 60 - 7 * playerCount),
      (CardFactory.createSilver(), 40),
      (CardFactory.createGold(), 30),
      (CardFactory.createEstate(), victoryCards),
      (CardFactory.createDuchy(), victoryCards),
      (CardFactory.createProvince(), victoryCards)
    )

    val actionPiles = actionCards.toList.map(card => (card: Card, 10))
    new PlayTable(basicCards ++ actionPiles)
  }
}
