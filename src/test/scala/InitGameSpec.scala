
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

class InitGameSpec extends AnyWordSpec with Matchers {

  "parseNumberInput" should {

    "accept integers within the specified range" in {
      parseNumberInput("2", 2, 8) shouldBe Some(2)
      parseNumberInput("5", 2, 8) shouldBe Some(5)
      parseNumberInput("8", 2, 8) shouldBe Some(8)
    }

    "reject integers below the specified range" in {
      parseNumberInput("0", 2, 8) shouldBe None
      parseNumberInput("1", 2, 8) shouldBe None
      parseNumberInput("-2", 2, 8) shouldBe None
    }

    "reject integers above the specified range" in {
      parseNumberInput("9", 2, 8) shouldBe None
    }

    "work with different ranges" in {
      parseNumberInput("1", 1, 20) shouldBe Some(1)
      parseNumberInput("20", 1, 20) shouldBe Some(20)
      parseNumberInput("21", 1, 20) shouldBe None
    }

    "reject text" in {
      parseNumberInput("abc", 2, 8) shouldBe None
    }

    "reject decimal numbers" in {
      parseNumberInput("3.5", 2, 8) shouldBe None
    }

    "reject empty input" in {
      parseNumberInput("", 2, 8) shouldBe None
    }
  }

  "readNumberInput" should {

    "return the number for valid input" in {
      val inputs = mutable.Queue("4")
      val messages = mutable.Queue.empty[String]

      val result = readNumberInput(
        () => inputs.dequeue(),
        message => messages.enqueue(message),
        2,
        8
      )

      result shouldBe 4
      messages should contain(
        "Enter an integer between 2 and 8:"
      )
    }

    "print an error for invalid input" in {
      val inputs = mutable.Queue("abc", "5")
      val messages = mutable.Queue.empty[String]

      val result = readNumberInput(
        () => inputs.dequeue(),
        message => messages.enqueue(message),
        2,
        8
      )

      result shouldBe 5
      messages should contain(
        "Invalid input! Enter an integer between 2 and 8.\n"
      )
    }

    "ask again after invalid input" in {
      val inputs = mutable.Queue("abc", "1", "9", "3")
      val messages = mutable.Queue.empty[String]

      val result = readNumberInput(
        () => inputs.dequeue(),
        message => messages.enqueue(message),
        2,
        8
      )

      result shouldBe 3

      messages.count(
        _ == "Enter an integer between 2 and 8:"
      ) shouldBe 4

      messages.count(
        _ == "Invalid input! Enter an integer between 2 and 8.\n"
      ) shouldBe 3
    }

    "use the specified range in messages and validation" in {
      val inputs = mutable.Queue("0", "15")
      val messages = mutable.Queue.empty[String]

      val result = readNumberInput(
        () => inputs.dequeue(),
        message => messages.enqueue(message),
        1,
        20
      )

      result shouldBe 15

      messages should contain(
        "Enter an integer between 1 and 20:"
      )

      messages should contain(
        "Invalid input! Enter an integer between 1 and 20.\n"
      )
    }
  }

  "InitGame" should {

    "initialize with the first player and selected playset" in {
      val inputs = mutable.Queue("3", "1")

      val game = new InitGame(
        readInput = () => inputs.dequeue(),
        printMessage = _ => ()
      )

      game.currentPlayer.id shouldBe 1
      game.selectedPlaySet shouldBe PlaySet.SizeDistortion
    }

    "return the next player after nextTurn" in {
      val inputs = mutable.Queue("3", "0")

      val game = new InitGame(
        readInput = () => inputs.dequeue(),
        printMessage = _ => ()
      )

      game.nextTurn()

      game.currentPlayer.id shouldBe 2
    }

    "return to the first player after the last player" in {
      val inputs = mutable.Queue("2", "0")

      val game = new InitGame(
        readInput = () => inputs.dequeue(),
        printMessage = _ => ()
      )

      game.nextTurn()
      game.currentPlayer.id shouldBe 2

      game.nextTurn()
      game.currentPlayer.id shouldBe 1
    }

    "support the minimum valid number of players" in {
      val inputs = mutable.Queue("2", "0")

      val game = new InitGame(
        readInput = () => inputs.dequeue(),
        printMessage = _ => ()
      )

      game.currentPlayer.id shouldBe 1

      game.nextTurn()
      game.currentPlayer.id shouldBe 2
    }

    "support the maximum valid number of players" in {
      val inputs = mutable.Queue("8", "5")

      val game = new InitGame(
        readInput = () => inputs.dequeue(),
        printMessage = _ => ()
      )

      game.currentPlayer.id shouldBe 1

      (1 to 7).foreach(_ => game.nextTurn())

      game.currentPlayer.id shouldBe 8

      game.nextTurn()
      game.currentPlayer.id shouldBe 1
    }

    "read the player count and playset independently" in {
      val inputs = mutable.Queue("4", "3")

      val game = new InitGame(
        readInput = () => inputs.dequeue(),
        printMessage = _ => ()
      )

      game.currentPlayer.id shouldBe 1
      game.selectedPlaySet shouldBe PlaySet.SleightOfHand
    }
  }
}
