package dominion.model

class VictoryCard(
                   name: String,
                   cost: Int,
                   val victoryPoints: Int
                 ) extends Card(name, cost) {

  require(victoryPoints >= 0,
    "Victory points cannot be negative")
}