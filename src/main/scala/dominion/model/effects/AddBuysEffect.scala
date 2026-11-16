package dominion.model.effects

import dominion.model.Player

final case class AddBuysEffect(amount: Int) extends CardEffect {
  require(amount >= 0, "Buy amount cannot be negative")

  override def execute(player: Player): Unit =
    player.addBuys(amount)
}
