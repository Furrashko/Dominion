package dominion.model.actioncard

import dominion.model.ActionCard
import dominion.model.effects.DrawCardsEffect

final class Smithy
  extends ActionCard(
    name = "Smithy",
    cost = 4,
    effects = List(
      DrawCardsEffect(3)
    )
  )