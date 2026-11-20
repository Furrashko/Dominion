
package dominion.controller.strategy

import dominion.controller.InitGameController
import dominion.model.Game
import dominion.view.InitGameView

class StandardGameStrategy(
                            readInput: () => String,
                            printMessage: String => Unit,
                            startGame: Game => Unit
                          ) extends MenuStrategy {

  override def execute(): Boolean = {

    val initController = new InitGameController(
      readInput = readInput
    )

    val initView = new InitGameView(
      printMessage = printMessage
    )

    initController.addObserver(initView)

    initController.run().foreach(startGame)

    true
  }
}
