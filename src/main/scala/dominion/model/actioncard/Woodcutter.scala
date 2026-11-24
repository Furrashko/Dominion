package dominion.model.actioncard

import dominion.model.ActionCard
import dominion.model.effects.{AddBuysEffect, AddMoneyEffect}

final class Woodcutter
  extends ActionCard(
    name = "Woodcutter",
    cost = 3,
    effects = List(
      AddMoneyEffect(2),
      AddBuysEffect(1)
    )
  )
