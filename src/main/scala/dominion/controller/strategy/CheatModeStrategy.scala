package dominion.controller.strategy

class CheatModeStrategy(
    printMessage: String => Unit
) extends MenuStrategy {

  override def execute(): Boolean = {
    // The actual cheat-mode rules are not implemented yet.
    printMessage("Cheat Mode selected.")
    true
  }
}
