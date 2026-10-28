import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

class InitGameSpec extends AnyWordSpec with Matchers {

  "parseNumberOfPlayers" should {

    "accept integers between 2 and 8" in {
      parseNumberOfPlayers("2") shouldBe Some(2)
      parseNumberOfPlayers("5") shouldBe Some(5)
      parseNumberOfPlayers("8") shouldBe Some(8)
    }

    "reject integers below 2" in {
      parseNumberOfPlayers("0") shouldBe None
      parseNumberOfPlayers("1") shouldBe None
      parseNumberOfPlayers("-2") shouldBe None
    }

    "reject integers above 8" in {
      parseNumberOfPlayers("9") shouldBe None
    }

    "reject text" in {
      parseNumberOfPlayers("abc") shouldBe None
    }

    "reject decimal numbers" in {
      parseNumberOfPlayers("3.5") shouldBe None
    }

    "reject empty input" in {
      parseNumberOfPlayers("") shouldBe None
    }
  }

  "readNumberOfPlayers" should {

    "return the number for valid input" in {
      val inputs = mutable.Queue("4")
      val messages = mutable.Queue.empty[String]

      val result = readNumberOfPlayers(
        () => inputs.dequeue(),
        message => messages.enqueue(message)
      )

      messages should contain("Enter number of Players:")
      result shouldBe 4
    }

    "print an error for invalid input" in {
      val inputs = mutable.Queue("abc", "5")
      val messages = mutable.Queue.empty[String]

      val result = readNumberOfPlayers(
        () => inputs.dequeue(),
        message => messages.enqueue(message)
      )

      result shouldBe 5
      messages should contain(
        "Invalid input! Enter an integer between 2 and 8."
      )
    }

    "ask again after invalid input" in {
      val inputs = mutable.Queue("abc", "1", "9", "3")
      val messages = mutable.Queue.empty[String]

      val result = readNumberOfPlayers(
        () => inputs.dequeue(),
        message => messages.enqueue(message)
      )

      result shouldBe 3

      messages.count(_ == "Enter number of Players:") shouldBe 4

      messages.count(
        _ == "Invalid input! Enter an integer between 2 and 8."
      ) shouldBe 3
    }
  }

  "InitGame" should {

    "initialize with the first player" in {
      val game = new InitGame(
        readInput = () => "3",
        printMessage = _ => ()
      )

      game.currentPlayer.id shouldBe 1
    }

    "return the next player after nextTurn" in {
      val game = new InitGame(
        readInput = () => "3",
        printMessage = _ => ()
      )

      game.nextTurn()

      game.currentPlayer.id shouldBe 2
    }

    "return to the first player after the last player" in {
      val game = new InitGame(
        readInput = () => "2",
        printMessage = _ => ()
      )

      game.nextTurn()
      game.currentPlayer.id shouldBe 2

      game.nextTurn()
      game.currentPlayer.id shouldBe 1
    }

    "support the minimum valid number of players" in {
      val game = new InitGame(
        readInput = () => "2",
        printMessage = _ => ()
      )

      game.currentPlayer.id shouldBe 1

      game.nextTurn()
      game.currentPlayer.id shouldBe 2
    }

    "support the maximum valid number of players" in {
      val game = new InitGame(
        readInput = () => "8",
        printMessage = _ => ()
      )

      game.currentPlayer.id shouldBe 1

      (1 to 7).foreach(_ => game.nextTurn())

      game.currentPlayer.id shouldBe 8

      game.nextTurn()
      game.currentPlayer.id shouldBe 1
    }
  }
}