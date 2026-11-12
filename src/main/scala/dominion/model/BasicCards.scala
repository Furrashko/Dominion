package dominion.model

object BasicCards {

  // Victory Cards
  val estate = new VictoryCard(
    name = "Estate",
    cost = 2,
    victoryPoints = 1
  )

  val duchy = new VictoryCard(
    name = "Duchy",
    cost = 5,
    victoryPoints = 3
  )

  val province = new VictoryCard(
    name = "Province",
    cost = 8,
    victoryPoints = 6
  )

  // Treasure Cards
  val copper = new TreasureCard(
    name = "Copper",
    cost = 0,
    value = 1
  )

  val silver = new TreasureCard(
    name = "Silver",
    cost = 3,
    value = 2
  )

  val gold = new TreasureCard(
    name = "Gold",
    cost = 6,
    value = 3
  )
}