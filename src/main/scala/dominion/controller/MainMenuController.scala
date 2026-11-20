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

  /*
  view soll nich von hier aufgerufen werden. es ruft obseerver auf
   */

  private val strategies: Map[String, MenuStrategy] = Map(
    "0" -> new CheatModeStrategy(printMessage),
    "1" -> new StandardGameStrategy(readInput, printMessage, startGame),
    "help" -> new HelpStrategy(printMessage),
    "exit" -> new ExitStrategy(printMessage)
  )

  def run(): Unit = {

    @tailrec
    def loop(): Unit = {
      //hier
      view.showMenu()

      Option(readInput()) match {
        case None =>
          //hier
          view.showMessage("Exiting Dominion...")

        case Some(input) =>
          strategies.get(input) match {
            case Some(strategy) =>
              if (strategy.execute())
                loop()

            case None =>
              if (input.isEmpty) {
                //qdf
                view.showMessage("Empty Input")
              } else {
                //asdf
                view.showMessage("Invalid/Unknown input")
              }
              loop()
          }
      }
    }

    loop()
  }
}
