package dominion.model.playset

import dominion.model.ActionCard

/**
 * A validated, reusable definition of one supply preset.
 *
 * Factories are used instead of card instances so every game receives fresh
 * ActionCard objects. Exactly ten differently named ActionCards are required.
 */
final class PlaySetDefinition(
                               val name: String,
                               val actionCardFactories: Vector[() => ActionCard]
                             ) {
  require(name.trim.nonEmpty, "PlaySet name cannot be empty")
  require(actionCardFactories.size == PlaySetDefinition.RequiredActionCardCount,
    s"A PlaySet must contain exactly ${PlaySetDefinition.RequiredActionCardCount} ActionCards")

  private val validationCards = actionCardFactories.map(_())
  private val names = validationCards.map(_.name)

  require(names.distinct.size == names.size,
    "A PlaySet must contain ten differently named ActionCards")

  def createActionCards: Vector[ActionCard] =
    actionCardFactories.map(_())

  def actionCardNames: Vector[String] =
    validationCards.map(_.name)
}

object PlaySetDefinition {
  val RequiredActionCardCount: Int = 10

  def custom(
              name: String,
              actionCardFactories: Seq[() => ActionCard]
            ): PlaySetDefinition =
    new PlaySetDefinition(name, actionCardFactories.toVector)
}
