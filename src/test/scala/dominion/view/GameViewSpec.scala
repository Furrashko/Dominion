
package dominion.view

import dominion.controller.{GameController, GameControllerEvent}
import dominion.model.*
import dominion.model.actioncard.Smithy
import dominion.util.PlaySet
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GameViewSpec extends AnyWordSpec with Matchers {

  private def createGame(): Game =
    new Game(
      Vector(Player(1), Player(2)),
      GameMode.StandardDeck,
      Some(PlaySet.FirstGame)
    )

  private def createView(
                          inputs: Iterator[String] = Iterator.empty
                        ): (GameView, Vector[String] => Vector[String]) = {
    var messages = Vector.empty[String]
    val view = new GameView(
      () => if (inputs.hasNext) inputs.next() else null,
      message => messages :+= message
    )
    (view, _ => messages)
  }

  "GameView" should {

    "attach itself to a controller" in {
      val game = createGame()
      val controller = new GameController(game, () => "quit")
      var messages = Vector.empty[String]

      val view = new GameView(
        () => null,
        message => messages :+= message
      )

      view.attach(controller)
      controller.execute("status")

      messages.exists(_.contains("DOMINION")) shouldBe true
    }

    "detach itself from the controller" in {
      val game = createGame()
      val controller = new GameController(game, () => "quit")
      var messages = Vector.empty[String]

      val view = new GameView(
        () => null,
        message => messages :+= message
      )

      view.attach(controller)
      view.detach()
      controller.execute("status")

      messages shouldBe empty
    }

    "render the current game" in {
      val (view, messages) = createView()
      val controller = new GameController(createGame(), () => "quit")
      view.attach(controller)

      view.render()

      messages(Vector.empty).exists(_.contains("DOMINION")) shouldBe true
    }

    "render the supply" in {
      val (view, messages) = createView()
      val controller = new GameController(createGame(), () => "quit")
      view.attach(controller)

      view.renderSupply()

      messages(Vector.empty).exists(_.contains("VORRAT")) shouldBe true
    }

    "render nothing when no game is attached" in {
      val (view, messages) = createView()

      view.render()
      view.renderSupply()

      messages(Vector.empty) shouldBe empty
    }

    "show help when receiving ShowHelp" in {
      val (view, messages) = createView()

      view.update(GameControllerEvent.ShowHelp)

      messages(Vector.empty).mkString should include("undo")
      messages(Vector.empty).mkString should include("redo")
      messages(Vector.empty).mkString should include("play")
    }

    "show the game when receiving ShowGame" in {
      val (view, messages) = createView()
      val controller = new GameController(createGame(), () => "quit")
      view.attach(controller)

      view.update(GameControllerEvent.ShowGame)

      messages(Vector.empty).exists(_.contains("DOMINION")) shouldBe true
    }

    "show a prompt" in {
      val (view, messages) = createView()

      view.update(GameControllerEvent.ShowPrompt("Prompt"))

      messages(Vector.empty) shouldBe Vector("Prompt")
    }

    "show a message" in {
      val (view, messages) = createView()

      view.update(GameControllerEvent.ShowMessage("Message"))

      messages(Vector.empty) shouldBe Vector("Message")
    }

    "render when the game is updated" in {
      val (view, messages) = createView()
      val controller = new GameController(createGame(), () => "quit")
      view.attach(controller)

      view.update(
        GameControllerEvent.GameChanged(GameUpdated("test"))
      )

      messages(Vector.empty).exists(_.contains("DOMINION")) shouldBe true
    }

    "not render when the game state is restored" in {
      val (view, messages) = createView()
      val controller = new GameController(createGame(), () => "quit")
      view.attach(controller)

      view.update(
        GameControllerEvent.GameChanged(GameStateRestored)
      )

      messages(Vector.empty) shouldBe empty
    }

    "ignore other game events" in {
      val (view, messages) = createView()
      val controller = new GameController(createGame(), () => "quit")
      view.attach(controller)

      view.update(
        GameControllerEvent.GameChanged(
          TurnPhaseChanged(controller.game.currentPlayer, TurnPhase.Buy)
        )
      )

      messages(Vector.empty) shouldBe empty
    }

    "choose the requested cards to discard" in {
      val cards = List(
        BasicCards.copper,
        BasicCards.silver,
        BasicCards.gold
      )
      val (view, _) = createView(Iterator("1,3"))

      val selected = view.chooseCardsToDiscard(
        Player(1),
        cards,
        2
      )

      selected shouldBe List(cards(0), cards(2))
    }

    "retry invalid discard selections" in {
      val cards = List(BasicCards.copper, BasicCards.silver)
      val (view, messages) = createView(
        Iterator("1,1", "1,2")
      )

      val selected = view.chooseCardsToDiscard(
        Player(1),
        cards,
        2
      )

      selected shouldBe cards
      messages(Vector.empty).exists(_.contains("Ungültige Auswahl")) shouldBe true
    }

    "use the default discard selection after input ends" in {
      val cards = List(BasicCards.copper, BasicCards.silver)
      val (view, _) = createView(Iterator(null))

      val selected = view.chooseCardsToDiscard(
        Player(1),
        cards,
        1
      )

      selected shouldBe List(cards.head)
    }

    "return no cards when discard count is zero" in {
      val (view, messages) = createView()

      view.chooseCardsToDiscard(
        Player(1),
        List(BasicCards.copper),
        0
      ) shouldBe empty

      messages(Vector.empty) shouldBe empty
    }

    "allow an empty Cellar selection" in {
      val (view, _) = createView(Iterator(""))

      view.chooseCellarDiscards(
        Player(1),
        List(BasicCards.copper)
      ) shouldBe empty
    }

    "return selected Cellar discards" in {
      val cards = List(BasicCards.copper, BasicCards.silver)
      val (view, _) = createView(Iterator("2"))

      view.chooseCellarDiscards(Player(1), cards) shouldBe
        List(BasicCards.silver)
    }

    "return no Cellar discards after input ends" in {
      val (view, _) = createView(Iterator(null))

      view.chooseCellarDiscards(
        Player(1),
        List(BasicCards.copper)
      ) shouldBe empty
    }

    "choose a card to trash" in {
      val cards = List(BasicCards.copper, BasicCards.silver)
      val (view, _) = createView(Iterator("2"))

      view.chooseCardToTrash(Player(1), cards) shouldBe
        Some(BasicCards.silver)
    }

    "allow declining to trash a card" in {
      val (view, _) = createView(Iterator(""))

      view.chooseCardToTrash(
        Player(1),
        List(BasicCards.copper)
      ) shouldBe None
    }

    "return None when there are no cards to trash" in {
      val (view, messages) = createView()

      view.chooseCardToTrash(Player(1), Nil) shouldBe None
      messages(Vector.empty) shouldBe empty
    }

    "choose a card to gain" in {
      val cards = List(BasicCards.copper, BasicCards.silver)
      val (view, _) = createView(Iterator("2"))

      view.chooseCardToGain(Player(1), cards, 3) shouldBe
        Some(BasicCards.silver)
    }

    "allow declining to gain a card" in {
      val (view, _) = createView(Iterator(""))

      view.chooseCardToGain(
        Player(1),
        List(BasicCards.copper),
        3
      ) shouldBe None
    }

    "return None when there are no cards to gain" in {
      val (view, messages) = createView()

      view.chooseCardToGain(Player(1), Nil, 3) shouldBe None
      messages(Vector.empty) shouldBe empty
    }

    "return None when gaining input ends" in {
      val (view, _) = createView(Iterator(null))

      view.chooseCardToGain(
        Player(1),
        List(BasicCards.copper),
        3
      ) shouldBe None
    }

    "answer yes to a Moat reveal prompt" in {
      val (view, _) = createView(Iterator("ja"))

      view.shouldRevealMoat(Player(2)) shouldBe true
    }

    "accept English yes answers to a Moat prompt" in {
      val (view, _) = createView(Iterator("yes"))

      view.shouldRevealMoat(Player(2)) shouldBe true
    }

    "accept the short yes answer to a Moat prompt" in {
      val (view, _) = createView(Iterator("y"))

      view.shouldRevealMoat(Player(2)) shouldBe true
    }

    "answer no to a Moat reveal prompt" in {
      val (view, _) = createView(Iterator("n"))

      view.shouldRevealMoat(Player(2)) shouldBe false
    }

    "answer no when Moat input ends" in {
      val (view, _) = createView(Iterator(null))

      view.shouldRevealMoat(Player(2)) shouldBe false
    }

    "render the hand and game resources" in {
      val game = createGame()
      game.currentPlayer.addCardToHand(BasicCards.copper)

      val rendered = GameView.render(game)

      rendered should include("Hand (1)")
      rendered should include("Copper")
      rendered should include("Aktionen:")
      rendered should include("Undo:")
      rendered should include("Redo:")
    }

    "render an empty hand" in {
      val rendered = GameView.render(createGame())

      rendered should include("(leer)")
    }

    "render played cards" in {
      val game = createGame()
      val card = new Smithy
      game.currentPlayer.addCardToHand(card)
      game.playAction(card)

      val rendered = GameView.render(game)

      rendered should include("Smithy")
    }

    "render an empty played-card area" in {
      val rendered = GameView.render(createGame())

      rendered should include("Gespielt: —")
    }

    "render supply sections" in {
      val rendered = GameView.renderSupply(createGame())

      rendered should include("Aktionskarten")
      rendered should include("Schatzkarten")
      rendered should include("Siegpunktkarten")
    }

    "render the empty supply message" in {
      val game = new Game(
        Vector(Player(1), Player(2)),
        GameMode.StandardDeck,
        None,
        new dominion.model.table.PlayTable(Nil)
      )

      GameView.renderSupply(game) should include(
        "Keine Karten mehr im Vorrat."
      )
    }

    "render the final score and winner when the game is over" in {
      val table = new dominion.model.table.PlayTable(
        List((BasicCards.province, 0), (BasicCards.copper, 10))
      )
      val game = new Game(
        Vector(Player(1), Player(2)),
        GameMode.StandardDeck,
        None,
        table
      )
      game.players.head.addCardToHand(BasicCards.estate)
      game.advancePhase()

      val winnerLine = GameView.render(game)
        .linesIterator.find(_.contains("SPIEL BEENDET")).get

      game.isGameOver shouldBe true
      winnerLine should include("Spieler 1")
      winnerLine should include("1 VP")
    }

    "render every winner when the scores are tied" in {
      val table = new dominion.model.table.PlayTable(
        List((BasicCards.province, 0), (BasicCards.copper, 10))
      )
      val game = new Game(
        Vector(Player(1), Player(2)),
        GameMode.StandardDeck,
        None,
        table
      )
      game.players.foreach(_.addCardToHand(BasicCards.estate))
      game.advancePhase()

      val winnerLine = GameView.render(game)
        .linesIterator.find(_.contains("SPIEL BEENDET")).get

      winnerLine should include("Spieler 1")
      winnerLine should include("Spieler 2")
      winnerLine should include("1 VP")
    }
  }
}
