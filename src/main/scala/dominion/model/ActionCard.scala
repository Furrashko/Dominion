package dominion.model

abstract class ActionCard(
                           name: String,
                           cost: Int
                         ) extends Card(name, cost)