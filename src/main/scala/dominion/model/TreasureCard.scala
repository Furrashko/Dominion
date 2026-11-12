package dominion.model

class TreasureCard(
                    name: String,
                    cost: Int,
                    val value: Int
                  ) extends Card(name, cost) {

  require(value >= 0,
    "Treasure value cannot be negative")
}