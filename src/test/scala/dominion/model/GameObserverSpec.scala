package dominion.model

import dominion.util.{Observable, Observer, PlaySet}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GameObserverSpec extends AnyWordSpec with Matchers {
  private def createGame(): Game =
    new Game(Vector(Player(1), Player(2)), GameMode.StandardDeck, Some(PlaySet.FirstGame))

  "Game observers" should {
    "be notified when the current player changes" in {
      val game = createGame()
      var events = Vector.empty[GameEvent]
      val observer = new Observer[GameEvent] {
        override def update(event: GameEvent): Unit = events :+= event
      }
      game.addObserver(observer)
      game.nextTurn()

      // Phase transition and GameUpdated notifications remain part of nextTurn's event contract.
      events.collectFirst {
        case CurrentPlayerChanged(player) => player
      } shouldBe Some(game.players(1))
    }

    "be notified when the phase changes" in {
      val game = createGame()
      var events = Vector.empty[GameEvent]
      val observer = new Observer[GameEvent] {
        override def update(event: GameEvent): Unit = events :+= event
      }
      game.addObserver(observer)
      game.advancePhase()
      events shouldBe Vector(
        TurnPhaseChanged(game.currentPlayer, TurnPhase.Buy),
        GameUpdated("advance-phase")
      )
    }

    "notify multiple observers" in {
      val game = createGame()
      var firstCount = 0
      var secondCount = 0
      val first = new Observer[GameEvent] {
        override def update(event: GameEvent): Unit = firstCount += 1
      }
      val second = new Observer[GameEvent] {
        override def update(event: GameEvent): Unit = secondCount += 1
      }
      game.addObserver(first)
      game.addObserver(second)
      game.advancePhase()
      firstCount shouldBe 2
      secondCount shouldBe 2
    }

    "not register the same observer more than once" in {
      val game = createGame()
      var count = 0
      val observer = new Observer[GameEvent] {
        override def update(event: GameEvent): Unit = count += 1
      }
      game.addObserver(observer)
      game.addObserver(observer)
      game.advancePhase()
      count shouldBe 2
    }

    "stop notifying an observer after it is removed" in {
      val game = createGame()
      var count = 0
      val observer = new Observer[GameEvent] {
        override def update(event: GameEvent): Unit = count += 1
      }
      game.addObserver(observer)
      game.removeObserver(observer)
      game.advancePhase()
      count shouldBe 0
    }

    "allow removing an observer multiple times" in {
      val game = createGame()
      var count = 0
      val observer = new Observer[GameEvent] {
        override def update(event: GameEvent): Unit = count += 1
      }
      game.addObserver(observer)
      game.removeObserver(observer)
      game.removeObserver(observer)
      game.advancePhase()
      count shouldBe 0
    }

    "allow registering an observer again after removal" in {
      val game = createGame()
      var count = 0
      val observer = new Observer[GameEvent] {
        override def update(event: GameEvent): Unit = count += 1
      }
      game.addObserver(observer)
      game.removeObserver(observer)
      game.addObserver(observer)
      game.advancePhase()
      count shouldBe 2
    }

    "not notify observers when an operation fails" in {
      val game = createGame()
      var events = Vector.empty[GameEvent]
      val observer = new Observer[GameEvent] {
        override def update(event: GameEvent): Unit = events :+= event
      }
      game.addObserver(observer)
      intercept[IllegalArgumentException] {
        game.playAction(new ActionCard("Foreign", 0) {})
      }
      events shouldBe empty
    }
  }

  "Observable" should {
    "support independent observer registrations" in {
      class TestObservable extends Observable[String] {
        def publish(value: String): Unit = notifyObservers(value)
      }
      val observable = new TestObservable
      var first = Vector.empty[String]
      var second = Vector.empty[String]
      val firstObserver = new Observer[String] {
        override def update(value: String): Unit = first :+= value
      }
      val secondObserver = new Observer[String] {
        override def update(value: String): Unit = second :+= value
      }
      observable.addObserver(firstObserver)
      observable.addObserver(secondObserver)
      observable.publish("test")
      first shouldBe Vector("test")
      second shouldBe Vector("test")
    }
  }
}
