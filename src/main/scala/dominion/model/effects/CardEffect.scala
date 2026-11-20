package dominion.model.effects

import dominion.model.Player

/**
 * Strategy interface for an individual action-card effect.
 */
trait CardEffect:
  def execute(player: Player): Unit
