package dominion.controller.strategy

import dominion.controller.InitGameController
import dominion.model.Game

class StandardGameStrategy(
    readInput: () => String,
    printMessage: String => Unit,
    startGame: Game => Unit
) extends MenuStrategy {

  override def execute(): Boolean = {
    val initController = new InitGameController(
      readInput = readInput,
      printMessage = printMessage
    )

    initController.run().foreach(startGame)
    true
  }
}
