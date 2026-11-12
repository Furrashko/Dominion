package dominion.view

import dominion.util.PlaySet
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class InitGameViewSpec extends AnyWordSpec with Matchers {

  "InitGameView" should {

    "show the player count prompt" in {
      var messages = Vector.empty[String]
      val view = new InitGameView(message => messages :+= message)

      view.showPlayerCountPrompt()

      messages shouldBe Vector("Number of Players:")
    }

    "show the selected player count" in {
      var messages = Vector.empty[String]
      val view = new InitGameView(message => messages :+= message)

      view.showSelectedPlayerCount(4)

      messages shouldBe Vector("Number of Players selected: 4\n")
    }

    "show the playset selection prompt" in {
      var messages = Vector.empty[String]
      val view = new InitGameView(message => messages :+= message)

      view.showPlaySetSelection()

      messages shouldBe Vector("Select Playset:")
    }

    "show all available playsets" in {
      var messages = Vector.empty[String]
      val view = new InitGameView(message => messages :+= message)

      view.showPlaySets()

      messages shouldBe PlaySet.values.map { playSet =>
        s"${playSet.id}: ${playSet.displayName}"
      }.toVector
    }

    "show the selected playset" in {
      var messages = Vector.empty[String]
      val view = new InitGameView(message => messages :+= message)

      view.showSelectedPlaySet(PlaySet.FirstGame)

      messages shouldBe Vector(
        s"Playset selected: ${PlaySet.FirstGame.displayName}\n"
      )
    }

    "show an invalid input message" in {
      var messages = Vector.empty[String]
      val view = new InitGameView(message => messages :+= message)

      view.showInvalidInput(2, 8)

      messages shouldBe Vector(
        "Invalid input! Enter an integer between 2 and 8.\n"
      )
    }
  }
}