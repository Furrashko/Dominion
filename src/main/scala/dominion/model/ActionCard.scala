package dominion.model

import dominion.model.effects.CardEffect

/**
 * Base class for every playable action card.
 *
 * The Player overload is retained for simple effects and source compatibility.
 * Cards that need access to the supply, opponents or player decisions override
 * play(context).
 */
abstract class ActionCard(
  name: String,
  cost: Int,
  val effects: List[CardEffect] = Nil
) extends Card(name, cost) {

  def play(player: Player): Unit =
    effects.foreach(_.execute(player))

  def play(context: ActionContext): Unit =
    play(context.player)
}
