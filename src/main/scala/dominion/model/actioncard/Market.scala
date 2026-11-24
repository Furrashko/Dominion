package dominion.model.actioncard

import dominion.model.ActionCard
import dominion.model.effects.{AddActionsEffect, DrawCardsEffect, AddBuysEffect, AddMoneyEffect}

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
