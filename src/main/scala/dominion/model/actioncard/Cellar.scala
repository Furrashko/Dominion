package dominion.model.actioncard

import dominion.model.*

final class Cellar extends ActionCard("Cellar", 2) {
  override def play(context: ActionContext): Unit = {
    val player = context.player
    player.addActions(1)

    val selected = context.decisions.chooseCellarDiscards(
      player,
      player.handCards
    )

    require(selected.size <= player.handSize,
      "Cellar cannot discard more cards than are in hand")
    context.discardFromHand(player, selected)
    player.drawCardsToHand(selected.size)
  }
}
