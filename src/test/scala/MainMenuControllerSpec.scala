package dominion.controller

import dominion.model.*
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class MainMenuControllerSpec extends AnyWordSpec with Matchers {

  "MainMenuController.run" should {

    "exit when the user enters exit" in {
      val inputs = Iterator("exit")
      var messages = Vector.empty[String]

      val controller = new MainMenuController(
        () => inputs.next(),
        message => messages :+= message,
        _ => ()
      )

      noException should be thrownBy controller.run()

      messages should contain("Exiting Dominion...")
    }

    "exit when the input is null" in {
      val controller = new MainMenuController(
        () => null,
        _ => (),
        _ => ()
      )

      noException should be thrownBy controller.run()
    }

    "display the help message and return to the menu" in {
      val inputs = Iterator("help", "exit")
      var messages = Vector.empty[String]

      val controller = new MainMenuController(
        () => inputs.next(),
        message => messages :+= message,
        _ => ()
      )

      controller.run()

      messages should contain("print help... feature coming soon™")
      messages.count(_ contains "Choose Game-mode") shouldBe 2
    }

    "select Cheat Mode and return to the menu" in {
      val inputs = Iterator("0", "exit")
      var messages = Vector.empty[String]

      val controller = new MainMenuController(
        () => inputs.next(),
        message => messages :+= message,
        _ => ()
      )

      controller.run()

      messages should contain("Cheat Mode selected.")
      messages.count(_ contains "Choose Game-mode") shouldBe 2
    }

    "show an error for unknown input" in {
      val inputs = Iterator("unknown", "exit")
      var messages = Vector.empty[String]

      val controller = new MainMenuController(
        () => inputs.next(),
        message => messages :+= message,
        _ => ()
      )

      controller.run()

      messages should contain("Invalid/Unknown input")
    }

    "show an error for empty input" in {
      val inputs = Iterator("", "exit")
      var messages = Vector.empty[String]

      val controller = new MainMenuController(
        () => inputs.next(),
        message => messages :+= message,
        _ => ()
      )

      controller.run()

      messages should contain("Empty Input")
    }

    "start a standard game and pass it to the callback" in {
      val inputs = Iterator("1", "4", "0", "exit")
      var startedGames = Vector.empty[Game]

      val controller = new MainMenuController(
        () => inputs.next(),
        _ => (),
        game => startedGames :+= game
      )

      controller.run()

      startedGames should have size 1
      startedGames.head.players should have size 4
      startedGames.head.gameMode shouldBe GameMode.StandardDeck
      startedGames.head.playSet shouldBe Some(PlaySet.FirstGame)
    }

    "return to the menu when standard game initialization is cancelled at player count" in {
      val inputs = Iterator("1", null, "exit")
      var startedGames = Vector.empty[Game]

      val controller = new MainMenuController(
        () => inputs.next(),
        _ => (),
        game => startedGames :+= game
      )

      controller.run()

      startedGames shouldBe empty
    }

    "return to the menu when standard game initialization is cancelled at playset selection" in {
      val inputs = Iterator("1", "2", null, "exit")
      var startedGames = Vector.empty[Game]

      val controller = new MainMenuController(
        () => inputs.next(),
        _ => (),
        game => startedGames :+= game
      )

      controller.run()

      startedGames shouldBe empty
    }
  }
}