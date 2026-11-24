package dominion.model.actioncard

import dominion.model.*

final class Workshop extends ActionCard("Workshop", 3) {
  override def play(context: ActionContext): Unit = {
    val player = context.player
    val gainable = context.table.availableCards.filter(_.cost <= 4)

    context.decisions
      .chooseCardToGain(player, gainable, 4)
      .filter(card => gainable.exists(_ eq card))
      .foreach(context.gainToDiscard)
  }
}
