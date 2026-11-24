
package dominion.controller

import dominion.model.*
import dominion.util.{Observer, PlaySet}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GameControllerSpec extends AnyWordSpec with Matchers {

  private class TestObserver extends Observer[GameControllerEvent] {
    var events = Vector.empty[GameControllerEvent]

    override def update(event: GameControllerEvent): Unit =
      events :+= event
  }

  private def createGame(): Game = {
    val players = Vector(Player(1), Player(2))
    new Game(players, GameMode.StandardDeck, Some(PlaySet.FirstGame))
  }

  private def createController(
                                game: Game = createGame(),
                                input: () => String = () => "quit",
                                decisions: DecisionProvider = DecisionProvider.default
                              ): (GameController, TestObserver) = {
    val controller = new GameController(game, input, decisions)
    val observer = new TestObserver
    controller.addObserver(observer)
    (controller, observer)
  }

  private def actionCard(name: String = "Test Action"): ActionCard =
    new ActionCard(name, 0) {}

  "GameController" should {

    "forward game events to its observers" in {
      val game = createGame()
      val (controller, observer) = createController(game)

      controller.advancePhase()

      observer.events should contain(
        GameControllerEvent.GameChanged(
          TurnPhaseChanged(game.currentPlayer, TurnPhase.Buy)
        )
      )
    }

    "play an action card from the current player's hand" in {
      val game = createGame()
      val card = actionCard()
      game.currentPlayer.addCardToHand(card)

      val (controller, observer) = createController(game)

      controller.playAction(1)

      game.currentPlayer.handCards should not contain card
      game.currentPlayer.playedCards should contain(card)

      observer.events should contain(
        GameControllerEvent.GameChanged(CardPlayed(game.currentPlayer, card))
      )
    }

    "play a treasure card during the Buy phase" in {
      val game = createGame()
      val card = BasicCards.copper
      game.currentPlayer.addCardToHand(card)
      game.advancePhase()

      val (controller, _) = createController(game)

      controller.playTreasure(1)

      game.currentPlayer.handCards should not contain card
      game.currentPlayer.playedCards should contain(card)
    }

    "play an action card through playHandCard" in {
      val game = createGame()
      val card = actionCard()
      game.currentPlayer.addCardToHand(card)

      val (controller, _) = createController(game)

      controller.playHandCard(1)

      game.currentPlayer.playedCards should contain(card)
    }

    "play a treasure card through playHandCard during Buy" in {
      val game = createGame()
      val card = BasicCards.copper
      game.currentPlayer.addCardToHand(card)
      game.advancePhase()

      val (controller, _) = createController(game)

      controller.playHandCard(1)

      game.currentPlayer.playedCards should contain(card)
    }

    "reject an invalid hand index" in {
      val (controller, _) = createController()

      an[IllegalArgumentException] should be thrownBy {
        controller.playHandCard(0)
      }
    }

    "reject an index larger than the hand size" in {
      val (controller, _) = createController()

      an[IllegalArgumentException] should be thrownBy {
        controller.playHandCard(1)
      }
    }

    "reject playing a treasure during Action phase" in {
      val game = createGame()
      game.currentPlayer.addCardToHand(BasicCards.copper)

      val (controller, _) = createController(game)

      an[IllegalArgumentException] should be thrownBy {
        controller.playHandCard(1)
      }
    }

    "reject playing an action card during Buy phase" in {
      val game = createGame()
      game.currentPlayer.addCardToHand(actionCard())
      game.advancePhase()

      val (controller, _) = createController(game)

      an[IllegalArgumentException] should be thrownBy {
        controller.playHandCard(1)
      }
    }

    "reject playing cards during Cleanup phase" in {
      val game = createGame()
      game.currentPlayer.addCardToHand(actionCard())
      game.advancePhase()
      game.advancePhase()

      val (controller, _) = createController(game)

      an[IllegalArgumentException] should be thrownBy {
        controller.playHandCard(1)
      }
    }

    "reject playing a treasure through playAction" in {
      val game = createGame()
      game.currentPlayer.addCardToHand(BasicCards.copper)

      val (controller, _) = createController(game)

      an[IllegalArgumentException] should be thrownBy {
        controller.playAction(1)
      }
    }

    "reject playing an action through playTreasure" in {
      val game = createGame()
      game.currentPlayer.addCardToHand(actionCard())
      game.advancePhase()

      val (controller, _) = createController(game)

      an[IllegalArgumentException] should be thrownBy {
        controller.playTreasure(1)
      }
    }

    "buy a card" in {
      val game = createGame()
      game.currentPlayer.addMoney(3)
      game.advancePhase()

      val (controller, observer) = createController(game)

      controller.buy("Silver")

      game.currentPlayer.discardPileSize shouldBe 1
      observer.events.exists {
        case GameControllerEvent.GameChanged(_: CardBought) => true
        case _ => false
      } shouldBe true
    }

    "advance the phase" in {
      val game = createGame()
      val (controller, _) = createController(game)

      controller.advancePhase()

      game.currentPhase shouldBe TurnPhase.Buy
    }

    "undo a game action" in {
      val game = createGame()
      val (controller, _) = createController(game)

      controller.advancePhase()
      controller.undo() shouldBe true

      game.currentPhase shouldBe TurnPhase.Action
    }

    "redo an undone game action" in {
      val game = createGame()
      val (controller, _) = createController(game)

      controller.advancePhase()
      controller.undo() shouldBe true
      controller.redo() shouldBe true

      game.currentPhase shouldBe TurnPhase.Buy
    }

    "return false when undo is unavailable" in {
      val (controller, _) = createController()

      controller.undo() shouldBe false
    }

    "return false when redo is unavailable" in {
      val (controller, _) = createController()

      controller.redo() shouldBe false
    }

    "execute the play command" in {
      val game = createGame()
      game.currentPlayer.addCardToHand(actionCard())
      val (controller, _) = createController(game)

      controller.execute("play 1") shouldBe true

      game.currentPlayer.playedCards should have size 1
    }

    "execute the treasure command" in {
      val game = createGame()
      game.currentPlayer.addCardToHand(BasicCards.copper)
      game.advancePhase()
      val (controller, _) = createController(game)

      controller.execute("treasure 1") shouldBe true

      game.currentPlayer.playedCards should contain(BasicCards.copper)
    }

    "execute the buy command" in {
      val game = createGame()
      game.currentPlayer.addMoney(3)
      game.advancePhase()
      val (controller, _) = createController(game)

      controller.execute("buy Silver") shouldBe true

      game.currentPlayer.discardPileSize shouldBe 1
    }

    "reject a buy command without a card name" in {
      val (controller, observer) = createController()

      controller.execute("buy") shouldBe true

      observer.events.exists {
        case GameControllerEvent.ShowMessage(message) =>
          message.contains("Usage: buy")
        case _ => false
      } shouldBe true
    }

    "execute the phase command" in {
      val game = createGame()
      val (controller, _) = createController(game)

      controller.execute("phase") shouldBe true

      game.currentPhase shouldBe TurnPhase.Buy
    }

    "execute the end command" in {
      val game = createGame()
      val (controller, _) = createController(game)

      controller.execute("end") shouldBe true

      game.currentPhase shouldBe TurnPhase.Buy
    }

    "execute the undo command" in {
      val game = createGame()
      val (controller, _) = createController(game)

      controller.advancePhase()
      controller.execute("undo") shouldBe true

      game.currentPhase shouldBe TurnPhase.Action
    }

    "show a message when undo is unavailable" in {
      val (controller, observer) = createController()

      controller.execute("undo") shouldBe true

      observer.events should contain(
        GameControllerEvent.ShowMessage(
          "Nichts zum Rückgängigmachen vorhanden."
        )
      )
    }

    "execute the redo command" in {
      val game = createGame()
      val (controller, _) = createController(game)

      controller.advancePhase()
      controller.undo()
      controller.execute("redo") shouldBe true

      game.currentPhase shouldBe TurnPhase.Buy
    }

    "show a message when redo is unavailable" in {
      val (controller, observer) = createController()

      controller.execute("redo") shouldBe true

      observer.events should contain(
        GameControllerEvent.ShowMessage(
          "Nichts zum Wiederholen vorhanden."
        )
      )
    }

    "execute the status command" in {
      val (controller, observer) = createController()

      controller.execute("status") shouldBe true

      observer.events should contain(GameControllerEvent.ShowGame)
    }

    "execute the show command" in {
      val (controller, observer) = createController()

      controller.execute("show") shouldBe true

      observer.events should contain(GameControllerEvent.ShowGame)
    }

    "execute the help command" in {
      val (controller, observer) = createController()

      controller.execute("help") shouldBe true

      observer.events should contain(GameControllerEvent.ShowHelp)
    }

    "report an unknown command" in {
      val (controller, observer) = createController()

      controller.execute("unknown") shouldBe true

      observer.events.exists {
        case GameControllerEvent.ShowMessage(message) =>
          message.contains("Unbekannter Befehl")
        case _ => false
      } shouldBe true
    }

    "accept an empty command" in {
      val (controller, observer) = createController()

      controller.execute("   ") shouldBe true

      observer.events shouldBe empty
    }

    "handle null commands" in {
      val (controller, observer) = createController()

      controller.execute(null) shouldBe true

      observer.events shouldBe empty
    }

    "handle commands case-insensitively" in {
      val game = createGame()
      val (controller, observer) = createController(game)

      controller.execute("HELP") shouldBe true

      observer.events should contain(GameControllerEvent.ShowHelp)
    }

    "handle invalid numeric arguments" in {
      val (controller, observer) = createController()

      controller.execute("play abc") shouldBe true

      observer.events.exists {
        case GameControllerEvent.ShowMessage(message) =>
          message.contains("Zug abgelehnt")
        case _ => false
      } shouldBe true
    }

    "stop executing after quit" in {
      val (controller, _) = createController()

      controller.execute("quit") shouldBe false
    }

    "stop executing after exit" in {
      val (controller, _) = createController()

      controller.execute("exit") shouldBe false
    }

    "run and publish the initial view events" in {
      val game = createGame()
      val inputs = Iterator("quit")
      val (controller, observer) =
        createController(game, () => inputs.next())

      controller.run()

      observer.events.head shouldBe GameControllerEvent.ShowHelp
      observer.events(1) shouldBe GameControllerEvent.ShowGame
      observer.events.exists {
        case GameControllerEvent.ShowPrompt(_) => true
        case _ => false
      } shouldBe true
    }

    "stop the input loop when input is null" in {
      val (controller, observer) =
        createController(input = () => null)

      controller.run()

      observer.events.count {
        case GameControllerEvent.ShowPrompt(_) => true
        case _ => false
      } shouldBe 1
    }

    "detach its game observer when closed" in {
      val game = createGame()
      val (controller, observer) = createController(game)

      controller.close()
      observer.events = Vector.empty

      game.advancePhase()

      observer.events shouldBe empty
    }

    "continue forwarding events before being closed" in {
      val game = createGame()
      val (controller, observer) = createController(game)

      controller.advancePhase()

      observer.events should not be empty
    }
  }
}
