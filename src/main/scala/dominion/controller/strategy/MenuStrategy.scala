package dominion.controller.strategy

/**
 * A menu strategy performs one menu action.
 *
 * @return true if the main menu should continue, false if it should terminate.
 */
trait MenuStrategy:
  def execute(): Boolean
