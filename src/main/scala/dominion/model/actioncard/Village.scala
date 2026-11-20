package dominion.model.actioncard

import dominion.model.ActionCard
import dominion.model.effects.{AddActionsEffect, DrawCardsEffect}

final class Village
  extends ActionCard(
    name = "Village",
    cost = 3,
    effects = List(
      DrawCardsEffect(1),
      AddActionsEffect(2)
    )
  )