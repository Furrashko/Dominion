package dominion.model.actioncard

import dominion.model.*

final class Militia extends ActionCard("Militia", 4) {
  override def play(context: ActionContext): Unit = {
    context.player.addMoney(2)

    context.game.players
      .filterNot(_ eq context.player)
      .foreach { opponent =>
        val hasMoat = opponent.handCards.exists(_.name == "Moat")
        val blocksAttack =
          hasMoat && context.decisions.shouldRevealMoat(opponent)

        if (!blocksAttack && opponent.handSize > 3) {
          val requiredDiscardCount = opponent.handSize - 3
          val selected = context.decisions.chooseCardsToDiscard(
            opponent,
            opponent.handCards,
            requiredDiscardCount
          )

          require(selected.size == requiredDiscardCount,
            "Militia requires the opponent to discard down to three cards")
          context.discardFromHand(opponent, selected)
        }
      }
  }
}
