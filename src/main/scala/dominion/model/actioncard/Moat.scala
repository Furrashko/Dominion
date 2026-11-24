package dominion.model.actioncard

import dominion.model.ActionCard
import dominion.model.effects.DrawCardsEffect

/**
 * Moat's attack-reaction is evaluated by attacking cards (currently Militia).
 */
final class Moat
  extends ActionCard(
    name = "Moat",
    cost = 2,
    effects = List(DrawCardsEffect(2))
  )
