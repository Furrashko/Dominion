package dominion.controller

import dominion.model.*
import dominion.util.PlaySet
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class InitGameControllerSpec extends AnyWordSpec with Matchers {

  "InitGameController.parseNumberInput" should {

    "accept a number within the allowed range" in {
      InitGameController.parseNumberInput("5", 2, 8) shouldBe Some(5)
    }

    "accept the minimum value" in {
      InitGameController.parseNumberInput("2", 2, 8) shouldBe Some(2)
    }

    "accept the maximum value" in {
      InitGameController.parseNumberInput("8", 2, 8) shouldBe Some(8)
    }

    "reject a number below the minimum" in {
      InitGameController.parseNumberInput("1", 2, 8) shouldBe None
    }

    "reject a number above the maximum" in {
      InitGameController.parseNumberInput("9", 2, 8) shouldBe None
    }

    "reject non-numeric input" in {
      InitGameController.parseNumberInput("abc", 2, 8) shouldBe None
    }

    "reject an empty string" in {
      InitGameController.parseNumberInput("", 2, 8) shouldBe None
    }

    "reject decimal input" in {
      InitGameController.parseNumberInput("2.5", 2, 8) shouldBe None
    }

    "reject whitespace around a number" in {
      InitGameController.parseNumberInput(" 5 ", 2, 8) shouldBe None
    }
  }

  "InitGameController.createGame" should {

    "create a game with two players" in {
      val controller = new InitGameController(() => "0", _ => ())

      val game = controller.createGame(
        2,
        GameMode.StandardDeck,
        Some(PlaySet.FirstGame)
      )

      game.players should have size 2
      game.players.map(_.id) shouldBe Vector(1, 2)
      game.gameMode shouldBe GameMode.StandardDeck
      game.playSet shouldBe Some(PlaySet.FirstGame)
    }

    "create a game with eight players" in {
      val controller = new InitGameController(() => "0", _ => ())

      val game = controller.createGame(
        8,
        GameMode.CheatMode,
        None
      )

      game.players should have size 8
      game.players.map(_.id) shouldBe (1 to 8).toVector
      game.gameMode shouldBe GameMode.CheatMode
      game.playSet shouldBe None
    }

    "reject fewer than two players" in {
      val controller = new InitGameController(() => "0", _ => ())

      an[IllegalArgumentException] should be thrownBy {
        controller.createGame(1, GameMode.StandardDeck, None)
      }
    }

    "reject more than eight players" in {
      val controller = new InitGameController(() => "0", _ => ())

      an[IllegalArgumentException] should be thrownBy {
        controller.createGame(9, GameMode.StandardDeck, None)
      }
    }
  }

  "InitGameController.run" should {

    "create a game with valid player count and playset input" in {
      val inputs = Iterator("4", "0")
      var messages = Vector.empty[String]

      val controller = new InitGameController(
        () => inputs.next(),
        message => messages :+= message
      )

      val result = controller.run()

      result shouldBe defined
      result.get.players should have size 4
      result.get.playSet shouldBe Some(PlaySet.FirstGame)
      result.get.gameMode shouldBe GameMode.StandardDeck
    }

    "create a game with the minimum player count" in {
      val inputs = Iterator("2", "1")

      val controller = new InitGameController(
        () => inputs.next(),
        _ => ()
      )

      val result = controller.run()

      result.get.players should have size 2
      result.get.playSet shouldBe Some(PlaySet.SizeDistortion)
    }

    "create a game with the maximum player count" in {
      val inputs = Iterator("8", "5")

      val controller = new InitGameController(
        () => inputs.next(),
        _ => ()
      )

      val result = controller.run()

      result.get.players should have size 8
      result.get.playSet shouldBe Some(PlaySet.SilverAndGold)
    }

    "retry after an invalid player count" in {
      val inputs = Iterator("abc", "1", "3", "0")
      var messages = Vector.empty[String]

      val controller = new InitGameController(
        () => inputs.next(),
        message => messages :+= message
      )

      val result = controller.run()

      result.get.players should have size 3
      messages should contain(
        "Invalid input! Enter an integer between 2 and 8.\n"
      )
    }

    "retry after an invalid playset selection" in {
      val inputs = Iterator("2", "invalid", "6", "2")
      var messages = Vector.empty[String]

      val controller = new InitGameController(
        () => inputs.next(),
        message => messages :+= message
      )

      val result = controller.run()

      result.get.playSet shouldBe Some(PlaySet.DeckTop)
      messages should contain(
        s"Invalid input! Enter an integer between 0 and ${PlaySet.values.length - 1}.\n"
      )
    }

    "return None when player count input is null" in {
      val controller = new InitGameController(
        () => null,
        _ => ()
      )

      controller.run() shouldBe None
    }

    "return None when playset input is null" in {
      val inputs = Iterator("2", null)

      val controller = new InitGameController(
        () => inputs.next(),
        _ => ()
      )

      controller.run() shouldBe None
    }
  }
}