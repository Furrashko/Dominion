package dominion.controller

import scala.annotation.tailrec

import dominion.controller.strategy.*
import dominion.model.Game
import dominion.util.Observable

enum MainMenuEvent {
  case ShowMenu
  case ShowMessage(message: String)
}

class MainMenuController(
                          readInput: () => String,
                          printMessage: String => Unit,
                          startGame: Game => Unit
                        ) extends Observable[MainMenuEvent] {

  private val strategies: Map[String, MenuStrategy] = Map(
    "0" -> new CheatModeStrategy(printMessage),
    "1" -> new StandardGameStrategy(readInput, printMessage, startGame),
    "help" -> new HelpStrategy(printMessage),
    "exit" -> new ExitStrategy(printMessage)
  )

  def run(): Unit = {

    @tailrec
    def loop(): Unit = {
      notifyObservers(MainMenuEvent.ShowMenu)

      Option(readInput()) match {
        case None =>
          notifyObservers(
            MainMenuEvent.ShowMessage("Exiting Dominion...")
          )

        case Some(input) =>
          strategies.get(input) match {
            case Some(strategy) =>
              if (strategy.execute())
                loop()

            case None =>
              val message =
                if (input.isEmpty) "Empty Input"
                else "Invalid/Unknown input"

              notifyObservers(MainMenuEvent.ShowMessage(message))
              loop()
          }
      }
    }

    loop()
  }
}