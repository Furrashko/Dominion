
package dominion.view

import dominion.controller.InitGameEvent
import dominion.model.playset.PlaySetCatalog
import dominion.util.PlaySet
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class InitGameViewSpec extends AnyWordSpec with Matchers {

  private def createView: (InitGameView, () => Vector[String]) = {
    var messages = Vector.empty[String]

    val view = new InitGameView(
      message => messages :+= message
    )

    (view, () => messages)
  }

  "InitGameView" should {

    "show the player count prompt" in {
      val (view, messages) = createView

      view.update(InitGameEvent.ShowPlayerCountPrompt)

      messages() shouldBe Vector("Number of Players:")
    }

    "show the selected player count" in {
      val (view, messages) = createView

      view.update(InitGameEvent.PlayerCountSelected(4))

      messages() shouldBe Vector(
        "Number of Players selected: 4\n"
      )
    }

    "show the playset selection prompt" in {
      val (view, messages) = createView

      view.update(InitGameEvent.ShowPlaySetSelection)

      messages() shouldBe Vector("Select Playset:")
    }

    "show only available PlaySets" in {
      val (view, messages) = createView

      view.update(InitGameEvent.ShowPlaySets)

      messages() shouldBe PlaySetCatalog.availablePlaySets.map {
        playSet =>
          s"${playSet.id}: ${playSet.displayName}"
      }
    }

    "show the selected PlaySet" in {
      val (view, messages) = createView

      view.update(
        InitGameEvent.PlaySetSelected(PlaySet.FirstGame)
      )

      messages() shouldBe Vector(
        s"Playset selected: ${PlaySet.FirstGame.displayName}\n"
      )
    }

    "show an invalid input message" in {
      val (view, messages) = createView

      view.update(InitGameEvent.InvalidInput(2, 8))

      messages() shouldBe Vector(
        "Invalid input! Enter an integer between 2 and 8.\n"
      )
    }

    "show invalid PlaySet input with available identifiers" in {
      val (view, messages) = createView

      view.update(
        InitGameEvent.InvalidPlaySetInput(Vector(0, 2, 4))
      )

      messages() shouldBe Vector(
        "Invalid PlaySet. Available PlaySet IDs: 0, 2, 4\n"
      )
    }

    "show none when no PlaySets are available" in {
      val (view, messages) = createView

      view.update(
        InitGameEvent.InvalidPlaySetInput(Vector.empty)
      )

      messages() shouldBe Vector(
        "Invalid PlaySet. Available PlaySet IDs: none\n"
      )
    }

    "show a message when no complete PlaySets are available" in {
      val (view, messages) = createView

      view.update(InitGameEvent.ShowPlaySets)

      PlaySetCatalog.availablePlaySets.nonEmpty shouldBe true
      messages().nonEmpty shouldBe true
    }
  }
}
