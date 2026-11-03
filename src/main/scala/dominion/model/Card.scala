package dominion.model

abstract class Card(
                     val name: String,
                     val cost: Int
                   ) {

  require(name.nonEmpty, "Card name cannot be empty")
  require(cost >= 0, "Card cost cannot be negative")
}