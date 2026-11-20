
package dominion.view

import dominion.controller.InitGameEvent
import dominion.util.PlaySet
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class InitGameViewSpec extends AnyWordSpec with Matchers {

  "InitGameView" should {

    "show the player count prompt" in {
      var messages = Vector.empty[String]
      val view = new InitGameView(message => messages :+= message)

      view.update(InitGameEvent.ShowPlayerCountPrompt)

      messages shouldBe Vector("Number of Players:")
    }

    "show the selected player count" in {
      var messages = Vector.empty[String]
      val view = new InitGameView(message => messages :+= message)

      view.update(InitGameEvent.PlayerCountSelected(4))

      messages shouldBe Vector("Number of Players selected: 4\n")
    }

    "show the playset selection prompt" in {
      var messages = Vector.empty[String]
      val view = new InitGameView(message => messages :+= message)

      view.update(InitGameEvent.ShowPlaySetSelection)

      messages shouldBe Vector("Select Playset:")
    }

    "show all available playsets" in {
      var messages = Vector.empty[String]
      val view = new InitGameView(message => messages :+= message)

      view.update(InitGameEvent.ShowPlaySets)

      messages shouldBe PlaySet.values.map { playSet =>
        s"${playSet.id}: ${playSet.displayName}"
      }.toVector
    }

    "show the selected playset" in {
      var messages = Vector.empty[String]
      val view = new InitGameView(message => messages :+= message)

      view.update(
        InitGameEvent.PlaySetSelected(PlaySet.FirstGame)
      )

      messages shouldBe Vector(
        s"Playset selected: ${PlaySet.FirstGame.displayName}\n"
      )
    }

    "show an invalid input message" in {
      var messages = Vector.empty[String]
      val view = new InitGameView(message => messages :+= message)

      view.update(InitGameEvent.InvalidInput(2, 8))

      messages shouldBe Vector(
        "Invalid input! Enter an integer between 2 and 8.\n"
      )
    }
  }
}
