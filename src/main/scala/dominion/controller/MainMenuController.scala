package dominion.controller

import scala.annotation.tailrec

import dominion.model.*
import dominion.view.MainMenuView

private enum MenuSelection:
  case CheatMode
  case StandardDeck
  case Help
  case Exit
  case Invalid

class MainMenuController(
                          readInput: () => String,
                          printMessage: String => Unit,
                          startGame: Game => Unit
                        ) {

  private val view = new MainMenuView(printMessage)

  private def parseMenuSelection(input: String): MenuSelection =
    input match {
      case "0"    => MenuSelection.CheatMode
      case "1"    => MenuSelection.StandardDeck
      case "help" => MenuSelection.Help
      case "exit" => MenuSelection.Exit
      case _      => MenuSelection.Invalid
    }

  private def startStandardGame(): Unit = {

    val initController = new InitGameController(
      readInput = readInput,
      printMessage = printMessage
    )

    initController.run().foreach(startGame)
  }

  def run(): Unit = {

    @tailrec
    def loop(): Unit = {

      view.showMenu()

      Option(readInput()) match {

        case None =>
          view.showMessage("Exiting Dominion...")

        case Some(input) =>

          parseMenuSelection(input) match {

            case MenuSelection.Exit =>
              view.showMessage("Exiting Dominion...")

            case MenuSelection.Help =>
              view.showMessage(
                "print help... feature coming soon™"
              )
              loop()

            case MenuSelection.CheatMode =>
              view.showMessage("Cheat Mode selected.")
              loop()

            case MenuSelection.StandardDeck =>
              startStandardGame()
              loop()

            case MenuSelection.Invalid =>
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