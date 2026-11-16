package dominion.controller.strategy

class ExitStrategy(
    printMessage: String => Unit
) extends MenuStrategy {

  override def execute(): Boolean = {
    printMessage("Exiting Dominion...")
    false
  }
}
