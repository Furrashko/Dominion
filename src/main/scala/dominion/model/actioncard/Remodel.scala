package dominion.model.actioncard

import dominion.model.*

final class Remodel extends ActionCard("Remodel", 4) {
  override def play(context: ActionContext): Unit = {
    val player = context.player
    context.decisions.chooseCardToTrash(player, player.handCards).foreach { selected =>
      require(player.handCards.exists(_ eq selected),
        "Remodel can only trash a card from the hand")

      val maximumCost = selected.cost + 2
      context.trashFromHand(player, selected)

      val gainable = context.table.availableCards.filter(_.cost <= maximumCost)
      context.decisions
        .chooseCardToGain(player, gainable, maximumCost)
        .filter(card => gainable.exists(_ eq card))
        .foreach(context.gainToDiscard)
    }
  }
}
