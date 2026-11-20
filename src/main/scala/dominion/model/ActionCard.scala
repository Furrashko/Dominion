package dominion.model

import dominion.model.effects.CardEffect

/**
 * An ActionCard delegates its behaviour to one or more CardEffects.
 * The empty default keeps simple/test ActionCard subclasses source-compatible.
 */
abstract class ActionCard(
                           name: String,
                           cost: Int,
                           val effects: List[CardEffect] = Nil
                         ) extends Card(name, cost) {

  def play(player: Player): Unit =
    effects.foreach(_.execute(player))
}
