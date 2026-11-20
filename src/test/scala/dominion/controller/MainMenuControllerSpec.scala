
package dominion.controller

import dominion.model.*
import dominion.util.{Observer, PlaySet}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MainMenuControllerSpec extends AnyWordSpec with Matchers {

  private class TestObserver extends Observer[MainMenuEvent] {
    var events = Vector.empty[MainMenuEvent]

    override def update(event: MainMenuEvent): Unit = {
      events :+= event
    }
  }

  private def createController(
                                inputs: Iterator[String],
                                messages: collection.mutable.Buffer[String],
                                observer: TestObserver
                              ): MainMenuController = {

    val controller = new MainMenuController(
      () => inputs.next(),
      message => messages += message,
      _ => ()
    )

    controller.addObserver(observer)
    controller
  }

  "MainMenuController.run" should {

    "display the menu before reading input" in {
      val observer = new TestObserver
      val controller = createController(
        Iterator("exit"),
        collection.mutable.ArrayBuffer.empty[String],
        observer
      )

      controller.run()

      observer.events.head shouldBe MainMenuEvent.ShowMenu
    }

    "exit when the user enters exit" in {
      val observer = new TestObserver
      val messages = collection.mutable.ArrayBuffer.empty[String]

      val controller = createController(
        Iterator("exit"),
        messages,
        observer
      )

      noException should be thrownBy controller.run()

      messages should contain("Exiting Dominion...")
      observer.events should contain(MainMenuEvent.ShowMenu)
    }

    "exit when the input is null" in {
      val observer = new TestObserver
      val messages = collection.mutable.ArrayBuffer.empty[String]

      val controller = createController(
        Iterator(null),
        messages,
        observer
      )

      controller.run()

      observer.events shouldBe Vector(
        MainMenuEvent.ShowMenu,
        MainMenuEvent.ShowMessage("Exiting Dominion...")
      )
    }

    "display the help message and return to the menu" in {
      val observer = new TestObserver
      val messages = collection.mutable.ArrayBuffer.empty[String]

      val controller = createController(
        Iterator("help", "exit"),
        messages,
        observer
      )

      controller.run()

      messages should contain("print help... feature coming soon™")

      observer.events.count(_ == MainMenuEvent.ShowMenu) shouldBe 2
    }

    "select Cheat Mode and return to the menu" in {
      val observer = new TestObserver
      val messages = collection.mutable.ArrayBuffer.empty[String]

      val controller = createController(
        Iterator("0", "exit"),
        messages,
        observer
      )

      controller.run()

      messages should contain("Cheat Mode selected.")

      observer.events.count(_ == MainMenuEvent.ShowMenu) shouldBe 2
    }

    "show an error for unknown input" in {
      val observer = new TestObserver
      val messages = collection.mutable.ArrayBuffer.empty[String]

      val controller = createController(
        Iterator("unknown", "exit"),
        messages,
        observer
      )

      controller.run()

      observer.events should contain(
        MainMenuEvent.ShowMessage("Invalid/Unknown input")
      )
    }

    "show an error for empty input" in {
      val observer = new TestObserver
      val messages = collection.mutable.ArrayBuffer.empty[String]

      val controller = createController(
        Iterator("", "exit"),
        messages,
        observer
      )

      controller.run()

      observer.events should contain(
        MainMenuEvent.ShowMessage("Empty Input")
      )
    }

    "start a standard game and pass it to the callback" in {
      val observer = new TestObserver
      val messages = collection.mutable.ArrayBuffer.empty[String]
      var startedGames = Vector.empty[Game]

      val inputs = Iterator("1", "4", "0", "exit")

      val controller = new MainMenuController(
        () => inputs.next(),
        message => messages += message,
        game => startedGames :+= game
      )

      controller.addObserver(observer)

      controller.run()

      startedGames should have size 1
      startedGames.head.players should have size 4
      startedGames.head.gameMode shouldBe GameMode.StandardDeck
      startedGames.head.playSet shouldBe Some(PlaySet.FirstGame)
    }

    "return to the menu when standard game initialization is cancelled at player count" in {
      val observer = new TestObserver
      val messages = collection.mutable.ArrayBuffer.empty[String]
      var startedGames = Vector.empty[Game]

      val inputs = Iterator("1", null, "exit")

      val controller = new MainMenuController(
        () => inputs.next(),
        message => messages += message,
        game => startedGames :+= game
      )

      controller.addObserver(observer)
      controller.run()

      startedGames shouldBe empty
    }

    "return to the menu when standard game initialization is cancelled at playset selection" in {
      val observer = new TestObserver
      val messages = collection.mutable.ArrayBuffer.empty[String]
      var startedGames = Vector.empty[Game]

      val inputs = Iterator("1", "2", null, "exit")

      val controller = new MainMenuController(
        () => inputs.next(),
        message => messages += message,
        game => startedGames :+= game
      )

      controller.addObserver(observer)
      controller.run()

      startedGames shouldBe empty
    }
  }
}
