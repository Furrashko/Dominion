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

    // Trigger validation before constructing any supply piles.
    val actionCards = definition.createActionCards
    require(actionCards.size == 10, "A PlaySet must contain exactly ten ActionCards")
    require(actionCards.map(_.name).distinct.size == 10,
      "A PlaySet must contain ten differently named ActionCards")

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
