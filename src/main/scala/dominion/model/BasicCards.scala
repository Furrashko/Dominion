
package dominion.model

object BasicCards {

  // Victory Cards
  val estate: VictoryCard =
    CardFactory.createEstate()

  val duchy: VictoryCard =
    CardFactory.createDuchy()

  val province: VictoryCard =
    CardFactory.createProvince()

  // Treasure Cards
  val copper: TreasureCard =
    CardFactory.createCopper()

  val silver: TreasureCard =
    CardFactory.createSilver()

  val gold: TreasureCard =
    CardFactory.createGold()
}
