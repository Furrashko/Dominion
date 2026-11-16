package dominion.model

import dominion.model.effects.*

/**
 * Example action cards demonstrating how effects can be composed.
 * These are card definitions; individual players still own their card piles.
 */
object ActionCards {

  final class Village
      extends ActionCard(
        name = "Village",
        cost = 3,
        effects = List(
          DrawCardsEffect(1),
          AddActionsEffect(2)
        )
      )

  final class Smithy
      extends ActionCard(
        name = "Smithy",
        cost = 4,
        effects = List(
          DrawCardsEffect(3)
        )
      )

  final class Market
      extends ActionCard(
        name = "Market",
        cost = 5,
        effects = List(
          DrawCardsEffect(1),
          AddActionsEffect(1),
          AddBuysEffect(1),
          AddMoneyEffect(1)
        )
      )
}
