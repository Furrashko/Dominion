package dominion.model

import dominion.util.Observer
import dominion.util.PlaySet
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GameObserverSpec extends AnyWordSpec with Matchers {

  "Game observers" should {

    "be notified when the current player changes" in {
      val game = new Game(
        Vector(Player(1), Player(2)),
        GameMode.StandardDeck,
        Some(PlaySet.FirstGame)
      )

      var receivedEvents = List.empty[GameEvent]

      val observer = new Observer[GameEvent] {
        override def update(event: GameEvent): Unit =
          receivedEvents = receivedEvents :+ event
      }

      game.addObserver(observer)
      game.nextTurn()

      receivedEvents shouldBe List(CurrentPlayerChanged(game.players(1)))
    }

    "stop notifying an observer after it is removed" in {
      val game = new Game(
        Vector(Player(1), Player(2)),
        GameMode.StandardDeck,
        Some(PlaySet.FirstGame)
      )

      var notificationCount = 0

      val observer = new Observer[GameEvent] {
        override def update(event: GameEvent): Unit =
          notificationCount += 1
      }

      game.addObserver(observer)
      game.removeObserver(observer)
      game.nextTurn()

      notificationCount shouldBe 0
    }
  }
}
