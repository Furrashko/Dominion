package dominion.model.effects

import dominion.model.Player

final case class AddMoneyEffect(amount: Int) extends CardEffect {
  require(amount >= 0, "Money amount cannot be negative")

  override def execute(player: Player): Unit =
    player.addMoney(amount)
}
