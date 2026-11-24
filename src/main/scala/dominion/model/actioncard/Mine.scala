package dominion.model.actioncard

import dominion.model.*

final class Mine extends ActionCard("Mine", 5) {
  override def play(context: ActionContext): Unit = {
    val player = context.player
    val treasures = player.handCards.collect { case card: TreasureCard => card }

    context.decisions.chooseCardToTrash(player, treasures).foreach { selected =>
      require(treasures.exists(_ eq selected),
        "Mine can only trash a TreasureCard from the hand")

      val maximumCost = selected.cost + 3
      context.trashFromHand(player, selected)

      val gainableTreasures = context.table.availableCards.collect {
        case treasure: TreasureCard if treasure.cost <= maximumCost => treasure
      }

      context.decisions
        .chooseCardToGain(player, gainableTreasures, maximumCost)
        .filter(card => gainableTreasures.exists(_ eq card))
        .foreach(context.gainToHand)
    }
  }
}
