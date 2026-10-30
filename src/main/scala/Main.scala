import scala.annotation.tailrec
import scala.io.StdIn.readLine

enum GameMode:
  case CheatMode
  case StandardDeck
  case Help
  case Invalid
  case Exit

def parseGameMode(input: String): GameMode =
  input match {
    case ""     => GameMode.Invalid
    case "0"    => GameMode.CheatMode
    case "1"    => GameMode.StandardDeck
    case "help" => GameMode.Help
    case "exit" => GameMode.Exit
    case _      => GameMode.Invalid
  }

def runGameMenu(
                 readInput: () => String,
                 printMessage: String => Unit,
                 startGame: GameMode => Unit
               ): Unit = {

  @tailrec
  def loop(): Unit = {
    printMessage(
      """-- Choose Game-mode: --
        |Type 0 for Cheat Mode.
        |Type 1 for Starter Deck.
        |Type "help" to get help.
        |Type "exit" to terminate the session.""".stripMargin
    )

    Option(readInput()) match {
      case None =>
        printMessage("Exiting Dominion...")

      case Some(input) =>
        parseGameMode(input) match {
          case GameMode.Exit =>
            printMessage("Exiting Dominion...")

          case GameMode.Help =>
            printMessage("print help... feature coming soon™")
            loop()

          case GameMode.CheatMode =>
            startGame(GameMode.CheatMode)
            loop()

          case GameMode.StandardDeck =>
            startGame(GameMode.StandardDeck)
            loop()

          case GameMode.Invalid =>
            if (input.isEmpty)
              printMessage("Empty Input")
            else
              printMessage("Invalid/Unknown input")

            loop()
        }
    }
  }

  loop()
}

// $COVERAGE-OFF$
@main def main(): Unit = {
  println("Dominion")

  runGameMenu(
    readInput = () => readLine(),
    printMessage = message => println(message),
    startGame = {
      case GameMode.CheatMode =>
        println("Cheat Mode selected.")

      case GameMode.StandardDeck =>
        val game = new InitGame(
          readInput = () => readLine(),
          printMessage = message => println(message)
        )

      case _ =>
        ()
    }
  )

  println("Game Terminated")
}
// $COVERAGE-ON$