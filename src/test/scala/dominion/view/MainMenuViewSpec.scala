
package dominion.view

import dominion.controller.MainMenuEvent
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MainMenuViewSpec extends AnyWordSpec with Matchers {

  "MainMenuView.update" should {

    "show the complete main menu when receiving ShowMenu" in {
      var messages = Vector.empty[String]
      val view = new MainMenuView(message => messages :+= message)

      view.update(MainMenuEvent.ShowMenu)

      messages should have size 1
      messages.head shouldBe
        """
          |-- Choose Game-mode: --
          |Type 0 for Cheat Mode.
          |Type 1 for Starter Deck.
          |Type "help" to get help.
          |Type "exit" to terminate the session.
          |""".stripMargin
    }

    "show a custom message when receiving ShowMessage" in {
      var messages = Vector.empty[String]
      val view = new MainMenuView(message => messages :+= message)

      view.update(MainMenuEvent.ShowMessage("Test message"))

      messages shouldBe Vector("Test message")
    }

    "show an empty message when receiving ShowMessage with empty content" in {
      var messages = Vector.empty[String]
      val view = new MainMenuView(message => messages :+= message)

      view.update(MainMenuEvent.ShowMessage(""))

      messages shouldBe Vector("")
    }

    "handle multiple events in the correct order" in {
      var messages = Vector.empty[String]
      val view = new MainMenuView(message => messages :+= message)

      view.update(MainMenuEvent.ShowMenu)
      view.update(MainMenuEvent.ShowMessage("Test"))
      view.update(MainMenuEvent.ShowMessage(""))

      messages should have size 3
      messages(0) should include("Choose Game-mode")
      messages(1) shouldBe "Test"
      messages(2) shouldBe ""
    }
  }
}
