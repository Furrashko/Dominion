package dominion.controller.strategy

class HelpStrategy(
    printMessage: String => Unit
) extends MenuStrategy {

  override def execute(): Boolean = {
    printMessage("print help... feature coming soon™")
    true
  }
}
