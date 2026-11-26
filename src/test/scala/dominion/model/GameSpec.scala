
package dominion.model

import dominion.model.table.PlayTable
import dominion.util.PlaySet
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GameSpec extends AnyWordSpec with Matchers {

  private def createGame(): Game =
    new Game(
      Vector(Player(1), Player(2)),
      GameMode.StandardDeck,
      Some(PlaySet.FirstGame)
    )

  private def failingAction(): ActionCard =
    new ActionCard("Failing Action", 0) {
      override def play(player: Player): Unit =
        throw new IllegalStateException("Action failed")
    }

  "A Game" should {

    "initialize with two players" in {
      val game = createGame()

      game.players should have size 2
      game.currentPlayer.id shouldBe 1
      game.currentPhase shouldBe TurnPhase.Action
      game.isGameOver shouldBe false
      game.canUndo shouldBe false
      game.canRedo shouldBe false
    }

    "initialize with eight players" in {
      val players = (1 to 8).map(Player(_)).toVector

      val game = new Game(
        players,
        GameMode.StandardDeck,
        None
      )

      game.players should have size 8
    }

    "allow a game without a playset" in {
      val game = new Game(
        Vector(Player(1), Player(2)),
        GameMode.CheatMode,
        None
      )

      game.playSet shouldBe None
    }

    "reject fewer than two players" in {
      an[IllegalArgumentException] should be thrownBy {
        new Game(
          Vector(Player(1)),
          GameMode.StandardDeck,
          None
        )
      }
    }

    "explain the player-count requirement in the error message" in {
      val exception = intercept[IllegalArgumentException] {
        new Game(
          Vector(Player(1)),
          GameMode.StandardDeck,
          None,
          new PlayTable(Nil)
        )
      }

      exception.getMessage should include("players")
    }

    "reject more than eight players" in {
      an[IllegalArgumentException] should be thrownBy {
        new Game(
          (1 to 9).map(Player(_)).toVector,
          GameMode.StandardDeck,
          None
        )
      }
    }

    "reject duplicate player IDs" in {
      an[IllegalArgumentException] should be thrownBy {
        new Game(
          Vector(Player(1), Player(1)),
          GameMode.StandardDeck,
          None
        )
      }
    }

    "advance from Action to Buy" in {
      val game = createGame()

      game.advancePhase()

      game.currentPhase shouldBe TurnPhase.Buy
    }

    "advance from Buy to Cleanup" in {
      val game = createGame()

      game.advancePhase()
      game.advancePhase()

      game.currentPhase shouldBe TurnPhase.Cleanup
    }

    "advance from Cleanup to the next player" in {
      val game = createGame()

      game.advancePhase()
      game.advancePhase()
      game.advancePhase()

      game.currentPlayer.id shouldBe 2
      game.currentPhase shouldBe TurnPhase.Action
    }

    "return to the first player after the last player" in {
      val game = createGame()

      game.nextTurn()
      game.nextTurn()

      game.currentPlayer.id shouldBe 1
    }

    "reset resources when advancing the turn" in {
      val game = createGame()
      val player = game.currentPlayer

      player.addActions(3)
      player.addMoney(5)
      player.addBuys(2)

      game.nextTurn()

      game.currentPlayer.perRoundActions shouldBe 1
      game.currentPlayer.perRoundMoney shouldBe 0
      game.currentPlayer.perRoundBuys shouldBe 1
    }

    "not draw a second initial hand for a player whose turn has not started" in {
      val game = createGame()

      game.players.foreach(_.handSize shouldBe 0)

      game.currentPlayer.addCardToHand(BasicCards.copper)
      game.nextTurn()

      game.currentPlayer.handSize shouldBe 0
    }

    "draw five cards when a player starts a subsequent turn" in {
      val players = Vector(Player(1), Player(2))

      players.foreach { player =>
        player.addCardsToDeck(
          List.fill(10)(BasicCards.copper)
        )
        player.drawCardsToHand(5)
      }

      val game = new Game(
        players,
        GameMode.StandardDeck,
        None
      )

      game.nextTurn()
      game.nextTurn()

      game.currentPlayer.handSize shouldBe 5
    }

    "publish a phase change event" in {
      val game = createGame()
      var events = Vector.empty[GameEvent]

      game.addObserver(new dominion.util.Observer[GameEvent] {
        override def update(event: GameEvent): Unit =
          events :+= event
      })

      game.advancePhase()

      events should contain(
        TurnPhaseChanged(game.currentPlayer, TurnPhase.Buy)
      )
    }

    "publish a player change event" in {
      val game = createGame()
      var events = Vector.empty[GameEvent]

      game.addObserver(new dominion.util.Observer[GameEvent] {
        override def update(event: GameEvent): Unit =
          events :+= event
      })

      game.advancePhase()
      game.advancePhase()
      game.advancePhase()

      events should contain(
        CurrentPlayerChanged(game.players(1))
      )
    }

    "publish GameUpdated after a successful operation" in {
      val game = createGame()
      var events = Vector.empty[GameEvent]

      game.addObserver(new dominion.util.Observer[GameEvent] {
        override def update(event: GameEvent): Unit =
          events :+= event
      })

      game.advancePhase()

      events.last shouldBe GameUpdated("advance-phase")
    }

    "play an action card" in {
      val game = createGame()
      val card = new ActionCard("Test", 0) {}

      game.currentPlayer.addCardToHand(card)

      game.playAction(card)

      game.currentPlayer.handCards should not contain card
      game.currentPlayer.playedCards should contain(card)
      game.currentPlayer.availableActions shouldBe 0
    }

    "publish events when playing an action card" in {
      val game = createGame()
      val card = new ActionCard("Test", 0) {}
      game.currentPlayer.addCardToHand(card)

      var events = Vector.empty[GameEvent]

      game.addObserver(new dominion.util.Observer[GameEvent] {
        override def update(event: GameEvent): Unit =
          events :+= event
      })

      game.playAction(card)

      events should contain(CardPlayed(game.currentPlayer, card))
      events should contain(ResourcesChanged(game.currentPlayer))
    }

    "reject an action card that is not in the hand" in {
      val game = createGame()

      an[IllegalArgumentException] should be thrownBy {
        game.playAction(new ActionCard("Foreign", 0) {})
      }
    }

    "reject an action card during Buy" in {
      val game = createGame()
      game.advancePhase()

      an[IllegalArgumentException] should be thrownBy {
        game.playAction(new ActionCard("Test", 0) {})
      }
    }

    "reject playing an action without available actions" in {
      val game = createGame()
      val card = new ActionCard("Test", 0) {}

      game.currentPlayer.addCardToHand(card)
      game.currentPlayer.useAction()

      an[IllegalArgumentException] should be thrownBy {
        game.playAction(card)
      }
    }

    "restore the state when an action effect fails" in {
      val game = createGame()
      val card = failingAction()
      game.currentPlayer.addCardToHand(card)

      val handBefore = game.currentPlayer.handCards
      val actionsBefore = game.currentPlayer.availableActions

      intercept[IllegalStateException] {
        game.playAction(card)
      }

      game.currentPlayer.handCards shouldBe handBefore
      game.currentPlayer.playedCards shouldBe empty
      game.currentPlayer.availableActions shouldBe actionsBefore
      game.canUndo shouldBe false
    }

    "publish no events when an action fails" in {
      val game = createGame()
      val action = failingAction()

      game.currentPlayer.addCardToHand(action)

      var events = Vector.empty[GameEvent]

      game.addObserver(new dominion.util.Observer[GameEvent] {
        override def update(event: GameEvent): Unit =
          events :+= event
      })

      intercept[IllegalStateException] {
        game.playAction(action)
      }

      events shouldBe empty
    }

    "play a treasure card during Buy" in {
      val game = createGame()
      val card = BasicCards.copper

      game.currentPlayer.addCardToHand(card)
      game.advancePhase()

      game.playTreasure(card)

      game.currentPlayer.playedCards should contain(card)
      game.currentPlayer.availableMoney shouldBe 1
    }

    "reject playing a treasure card during Action" in {
      val game = createGame()
      val card = BasicCards.copper

      game.currentPlayer.addCardToHand(card)

      an[IllegalArgumentException] should be thrownBy {
        game.playTreasure(card)
      }
    }

    "buy a card from the supply" in {
      val game = createGame()

      game.advancePhase()
      game.currentPlayer.addMoney(3)

      game.buyCard("Silver")

      game.currentPlayer.discardPileSize shouldBe 1
      game.currentPlayer.availableBuys shouldBe 0
      game.currentPlayer.availableMoney shouldBe 0
    }

    "accept card names without case sensitivity" in {
      val game = createGame()

      game.advancePhase()
      game.currentPlayer.addMoney(3)

      game.buyCard("sIlVeR")

      game.currentPlayer.discardPileSize shouldBe 1
    }

    "reject an unknown card pile" in {
      val game = createGame()
      game.advancePhase()
      game.currentPlayer.addMoney(10)

      an[IllegalArgumentException] should be thrownBy {
        game.buyCard("Unknown")
      }
    }

    "reject a purchase without enough money" in {
      val game = createGame()
      game.advancePhase()

      an[IllegalArgumentException] should be thrownBy {
        game.buyCard("Gold")
      }
    }

    "reject a purchase without available buys" in {
      val game = createGame()
      game.advancePhase()
      game.currentPlayer.addMoney(10)
      game.currentPlayer.useBuy()

      an[IllegalArgumentException] should be thrownBy {
        game.buyCard("Silver")
      }
    }

    "reject a purchase during Action" in {
      val game = createGame()
      game.currentPlayer.addMoney(10)

      an[IllegalArgumentException] should be thrownBy {
        game.buyCard("Silver")
      }
    }

    "publish purchase events" in {
      val game = createGame()
      game.advancePhase()
      game.currentPlayer.addMoney(3)

      var events = Vector.empty[GameEvent]

      game.addObserver(new dominion.util.Observer[GameEvent] {
        override def update(event: GameEvent): Unit =
          events :+= event
      })

      game.buyCard("Silver")

      events.exists(_.isInstanceOf[CardBought]) shouldBe true
      events.exists(_.isInstanceOf[SupplyChanged]) shouldBe true
      events.exists(_.isInstanceOf[ResourcesChanged]) shouldBe true
    }

    "undo a phase change" in {
      val game = createGame()

      game.advancePhase()
      game.canUndo shouldBe true

      game.undo() shouldBe true

      game.currentPhase shouldBe TurnPhase.Action
      game.canRedo shouldBe true
    }

    "redo a phase change" in {
      val game = createGame()

      game.advancePhase()
      game.undo()
      game.redo() shouldBe true

      game.currentPhase shouldBe TurnPhase.Buy
      game.canUndo shouldBe true
    }

    "return false when undo is unavailable" in {
      createGame().undo() shouldBe false
    }

    "return false when redo is unavailable" in {
      createGame().redo() shouldBe false
    }

    "clear the redo history after a new operation" in {
      val game = createGame()

      game.advancePhase()
      game.undo()
      game.advancePhase()

      game.canRedo shouldBe false
      game.redo() shouldBe false
    }

    "restore player resources when undoing" in {
      val game = createGame()

      game.currentPlayer.addMoney(5)
      game.advancePhase()

      game.undo()

      game.currentPlayer.perRoundMoney shouldBe 5
    }

    "restore the supply when undoing a purchase" in {
      val game = createGame()
      game.advancePhase()
      game.currentPlayer.addMoney(3)

      val initialCount = game.playTable.getPile("Silver").get.count

      game.buyCard("Silver")
      game.undo()

      game.playTable.getPile("Silver").get.count shouldBe initialCount
      game.currentPlayer.discardPileSize shouldBe 0
    }

    "restore the hand when undoing a card play" in {
      val game = createGame()
      val card = new ActionCard("Test", 0) {}

      game.currentPlayer.addCardToHand(card)
      game.playAction(card)
      game.undo()

      game.currentPlayer.handCards should contain(card)
      game.currentPlayer.playedCards should not contain card
    }

    "publish state restoration events on undo" in {
      val game = createGame()
      game.advancePhase()

      var events = Vector.empty[GameEvent]

      game.addObserver(new dominion.util.Observer[GameEvent] {
        override def update(event: GameEvent): Unit =
          events :+= event
      })

      game.undo()

      events should contain(GameStateRestored)
      events should contain(GameUpdated("undo"))
    }

    "publish state restoration events on redo" in {
      val game = createGame()
      game.advancePhase()
      game.undo()

      var events = Vector.empty[GameEvent]

      game.addObserver(new dominion.util.Observer[GameEvent] {
        override def update(event: GameEvent): Unit =
          events :+= event
      })

      game.redo()

      events should contain(GameStateRestored)
      events should contain(GameUpdated("redo"))
    }

    "calculate current scores" in {
      val game = createGame()

      game.players.head.addCardToHand(BasicCards.estate)

      game.currentScores shouldBe Map(1 -> 1, 2 -> 0)
    }

    "end when the Province pile is empty" in {
      val table = new PlayTable(
        List(
          (BasicCards.province, 0),
          (BasicCards.copper, 1),
          (BasicCards.silver, 1)
        )
      )

      val game = new Game(
        Vector(Player(1), Player(2)),
        GameMode.StandardDeck,
        None,
        table
      )

      var events = Vector.empty[GameEvent]

      game.addObserver(new dominion.util.Observer[GameEvent] {
        override def update(event: GameEvent): Unit =
          events :+= event
      })

      game.advancePhase()

      game.isGameOver shouldBe true
      events.exists(_.isInstanceOf[GameEnded]) shouldBe true
    }

    "reject actions after the game has ended" in {
      val table = new PlayTable(
        List((BasicCards.province, 0))
      )

      val game = new Game(
        Vector(Player(1), Player(2)),
        GameMode.StandardDeck,
        None,
        table
      )

      game.advancePhase()

      an[IllegalArgumentException] should be thrownBy {
        game.advancePhase()
      }
    }

    "reject playing a treasure that is not in the hand" in {
      val game = createGame()
      game.advancePhase()

      an[IllegalArgumentException] should be thrownBy {
        game.playTreasure(BasicCards.copper)
      }
    }

    "draw a new hand on the second turn when advancing through cleanup" in {
      val players = Vector(Player(1), Player(2))
      players.foreach { player =>
        player.addCardsToDeck(List.fill(10)(BasicCards.copper))
      }

      val game = new Game(players, GameMode.StandardDeck, None)

      var events = Vector.empty[GameEvent]
      game.addObserver(new dominion.util.Observer[GameEvent] {
        override def update(event: GameEvent): Unit =
          events :+= event
      })

      (1 to 6).foreach(_ => game.advancePhase())

      game.currentPlayer.id shouldBe 1
      game.currentPlayer.handSize shouldBe 5
      events should contain(
        CardDrawn(game.currentPlayer, game.currentPlayer.handCards)
      )
    }
  }
}
