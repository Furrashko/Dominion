
package dominion.controller

import dominion.model.*
import dominion.util.{Observer, PlaySet}
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class InitGameControllerSpec extends AnyWordSpec with Matchers {

  private class TestObserver extends Observer[InitGameEvent] {
    var events = Vector.empty[InitGameEvent]

    override def update(event: InitGameEvent): Unit =
      events :+= event
  }

  private def controller(
                          inputs: Iterator[String]
                        ): (InitGameController, TestObserver) = {

    val instance = new InitGameController(
      () => inputs.next()
    )

    val observer = new TestObserver
    instance.addObserver(observer)

    (instance, observer)
  }

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
      val instance = new InitGameController(() => "0")

      val game = instance.createGame(
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
      val instance = new InitGameController(() => "0")

      val game = instance.createGame(
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
      val instance = new InitGameController(() => "0")

      an[IllegalArgumentException] should be thrownBy {
        instance.createGame(1, GameMode.StandardDeck, None)
      }
    }

    "reject more than eight players" in {
      val instance = new InitGameController(() => "0")

      an[IllegalArgumentException] should be thrownBy {
        instance.createGame(9, GameMode.StandardDeck, None)
      }
    }
  }

  "InitGameController.run" should {

    "create a game with valid player count and playset input" in {
      val (instance, observer) = controller(
        Iterator("4", "0")
      )

      val result = instance.run()

      result shouldBe defined
      result.get.players should have size 4
      result.get.playSet shouldBe Some(PlaySet.FirstGame)
      result.get.gameMode shouldBe GameMode.StandardDeck

      observer.events should contain(
        InitGameEvent.PlayerCountSelected(4)
      )

      observer.events should contain(
        InitGameEvent.PlaySetSelected(PlaySet.FirstGame)
      )
    }

    "create a game with the minimum player count" in {
      val (instance, _) = controller(
        Iterator("2", "1")
      )

      val result = instance.run()

      result.get.players should have size 2
      result.get.playSet shouldBe Some(PlaySet.SizeDistortion)
    }

    "create a game with the maximum player count" in {
      val (instance, _) = controller(
        Iterator("8", "5")
      )

      val result = instance.run()

      result.get.players should have size 8
      result.get.playSet shouldBe Some(PlaySet.SilverAndGold)
    }

    "publish an invalid input event and retry after invalid player count" in {
      val (instance, observer) = controller(
        Iterator("abc", "1", "3", "0")
      )

      val result = instance.run()

      result.get.players should have size 3

      observer.events should contain(
        InitGameEvent.InvalidInput(2, 8)
      )
    }

    "publish an invalid input event and retry after invalid playset selection" in {
      val (instance, observer) = controller(
        Iterator("2", "invalid", "6", "2")
      )

      val result = instance.run()

      result.get.playSet shouldBe Some(PlaySet.DeckTop)

      observer.events should contain(
        InitGameEvent.InvalidInput(0, PlaySet.values.length - 1)
      )
    }

    "publish the initial prompt events" in {
      val (instance, observer) = controller(
        Iterator("2", "0")
      )

      instance.run()

      observer.events should contain(
        InitGameEvent.ShowPlayerCountPrompt
      )

      observer.events should contain(
        InitGameEvent.ShowPlaySetSelection
      )

      observer.events should contain(
        InitGameEvent.ShowPlaySets
      )
    }

    "return None when player count input is null" in {
      val (instance, _) = controller(
        Iterator(null)
      )

      instance.run() shouldBe None
    }

    "return None when playset input is null" in {
      val (instance, _) = controller(
        Iterator("2", null)
      )

      instance.run() shouldBe None
    }
  }
}
