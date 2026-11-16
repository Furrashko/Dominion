package dominion.controller

import scala.annotation.tailrec

import dominion.controller.strategy.*
import dominion.model.Game
import dominion.view.MainMenuView

class MainMenuController(
                          readInput: () => String,
                          printMessage: String => Unit,
                          startGame: Game => Unit
                        ) {

  private val view = new MainMenuView(printMessage)

  private val strategies: Map[String, MenuStrategy] = Map(
    "0" -> new CheatModeStrategy(printMessage),
    "1" -> new StandardGameStrategy(readInput, printMessage, startGame),
    "help" -> new HelpStrategy(printMessage),
    "exit" -> new ExitStrategy(printMessage)
  )

  def run(): Unit = {

    @tailrec
    def loop(): Unit = {
      view.showMenu()

      Option(readInput()) match {
        case None =>
          view.showMessage("Exiting Dominion...")

        case Some(input) =>
          strategies.get(input) match {
            case Some(strategy) =>
              if (strategy.execute())
                loop()

            case None =>
              if (input.isEmpty)
                view.showMessage("Empty Input")
              else
                view.showMessage("Invalid/Unknown input")
              loop()
          }
      }
    }

    loop()
  }
}
