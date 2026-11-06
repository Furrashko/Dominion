package dominion.view

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class MainMenuViewSpec extends AnyWordSpec with Matchers {

  "MainMenuView" should {

    "show the complete main menu" in {
      var messages = Vector.empty[String]
      val view = new MainMenuView(message => messages :+= message)

      view.showMenu()

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

    "show a custom message" in {
      var messages = Vector.empty[String]
      val view = new MainMenuView(message => messages :+= message)

      view.showMessage("Test message")

      messages shouldBe Vector("Test message")
    }

    "show an empty message" in {
      var messages = Vector.empty[String]
      val view = new MainMenuView(message => messages :+= message)

      view.showMessage("")

      messages shouldBe Vector("")
    }
  }
}