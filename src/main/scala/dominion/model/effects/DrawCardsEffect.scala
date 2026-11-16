package dominion.model.effects

import dominion.model.Player

final case class DrawCardsEffect(amount: Int) extends CardEffect {
  require(amount >= 0, "Draw amount cannot be negative")

  override def execute(player: Player): Unit = {
    player.drawCardsToHand(amount)
    ()
  }
}
