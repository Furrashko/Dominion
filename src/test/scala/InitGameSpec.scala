import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class InitGameSpec extends AnyWordSpec with Matchers {
  "parseGameMode" should {

    "return CheatMode for input 0" in {
      parseGameMode("0") shouldBe GameMode.CheatMode
    }

    "return StarterDeck for input 1" in {
      parseGameMode("1") shouldBe GameMode.StandardDeck
    }

    "return Help for input help" in {
      parseGameMode("help") shouldBe GameMode.Help
    }

    "return Exit for input exit" in {
      parseGameMode("exit") shouldBe GameMode.Exit
    }

    "return Invalid for empty input" in {
      parseGameMode("") shouldBe GameMode.Invalid
    }

    "return Invalid for unknown input" in {
      parseGameMode("invalid") shouldBe GameMode.Invalid
    }

    "return Invalid for whitespace input" in {
      parseGameMode(" ") shouldBe GameMode.Invalid
    }
  }

  "runGameMenu" should {

    "exit when the user enters exit" in {
      val inputs = Iterator("exit")
      var messages = List.empty[String]
      var startedModes = List.empty[GameMode]

      runGameMenu(
        readInput = () => inputs.next(),
        printMessage = message => messages = messages :+ message,
        startGame = mode => startedModes = startedModes :+ mode
      )

      messages should contain("Exiting Dominion…")
      startedModes shouldBe empty
    }

    "exit when the input stream reaches EOF" in {
      var messages = List.empty[String]
      var startedModes = List.empty[GameMode]

      runGameMenu(
        readInput = () => null,
        printMessage = message => messages = messages :+ message,
        startGame = mode => startedModes = startedModes :+ mode
      )

      messages should contain("Exiting Dominion…")
      startedModes shouldBe empty
    }

    "print help and continue until exit" in {
      val inputs = Iterator("help", "exit")
      var messages = List.empty[String]

      runGameMenu(
        readInput = () => inputs.next(),
        printMessage = message => messages = messages :+ message,
        startGame = _ => fail("No game should be started")
      )

      messages should contain("print help… feature coming soon™")
      messages should contain("Exiting Dominion…")
    }

    "report empty input and continue until exit" in {
      val inputs = Iterator("", "exit")
      var messages = List.empty[String]

      runGameMenu(
        readInput = () => inputs.next(),
        printMessage = message => messages = messages :+ message,
        startGame = _ => fail("No game should be started")
      )

      messages should contain("Empty Input")
      messages should contain("Exiting Dominion…")
    }

    "report invalid input and continue until exit" in {
      val inputs = Iterator("abc", "exit")
      var messages = List.empty[String]

      runGameMenu(
        readInput = () => inputs.next(),
        printMessage = message => messages = messages :+ message,
        startGame = _ => fail("No game should be started")
      )

      messages should contain("Invalid/Unknown input")
      messages should contain("Exiting Dominion…")
    }

    "start Cheat Mode and continue until exit" in {
      val inputs = Iterator("0", "exit")
      var startedModes = List.empty[GameMode]

      runGameMenu(
        readInput = () => inputs.next(),
        printMessage = _ => (),
        startGame = mode => startedModes = startedModes :+ mode
      )

      startedModes shouldBe List(GameMode.CheatMode)
    }

    "start Starter Deck and continue until exit" in {
      val inputs = Iterator("1", "exit")
      var startedModes = List.empty[GameMode]

      runGameMenu(
        readInput = () => inputs.next(),
        printMessage = _ => (),
        startGame = mode => startedModes = startedModes :+ mode
      )

      startedModes shouldBe List(GameMode.StandardDeck)
    }
  }

  "initGame" should {

    "read a valid player count and print it" in {
      val inputs = Iterator("4")
      var messages = List.empty[String]

      initGame(
        readInput = () => inputs.next(),
        printMessage = message => messages = messages :+ message
      )

      messages should contain("Enter number of Players:")
      messages should contain("Number of Players: 4")
    }

    "retry after invalid input before initializing the game" in {
      val inputs = Iterator("abc", "1", "5")
      var messages = List.empty[String]

      initGame(
        readInput = () => inputs.next(),
        printMessage = message => messages = messages :+ message
      )

      messages should contain("Invalid input! Enter an integer between 2 and 8.")
      messages should contain("Number of Players: 5")
    }

    "accept the minimum player count" in {
      val inputs = Iterator("2")
      var messages = List.empty[String]

      initGame(
        readInput = () => inputs.next(),
        printMessage = message => messages = messages :+ message
      )

      messages should contain("Number of Players: 2")
    }

    "accept the maximum player count" in {
      val inputs = Iterator("8")
      var messages = List.empty[String]

      initGame(
        readInput = () => inputs.next(),
        printMessage = message => messages = messages :+ message
      )

      messages should contain("Number of Players: 8")
    }
  }
}
