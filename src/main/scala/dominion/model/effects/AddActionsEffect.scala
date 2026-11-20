package dominion.model.effects

import dominion.model.Player

final case class AddActionsEffect(amount: Int) extends CardEffect {
  require(amount >= 0, "Action amount cannot be negative")

  override def execute(player: Player): Unit =
    player.addActions(amount)
}
