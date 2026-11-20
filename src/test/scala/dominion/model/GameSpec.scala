
package dominion.model

import dominion.util.PlaySet
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GameSpec extends AnyWordSpec with Matchers {

  "A Game" should {

    "initialize with two players" in {
      val players = Vector(
        Player(1),
        Player(2)
      )

      val game = new Game(
        players,
        GameMode.StandardDeck,
        Some(PlaySet.FirstGame)
      )

      game.players.size shouldBe 2
      game.currentPlayer.id shouldBe 1
      game.currentPhase shouldBe TurnPhase.Action
    }

    "initialize with eight players" in {
      val players =
        (1 to 8).map(Player(_)).toVector

      val game = new Game(
        players,
        GameMode.StandardDeck,
        Some(PlaySet.FirstGame)
      )

      game.players.size shouldBe 8
    }

    "allow a game without a playset" in {
      val game = new Game(
        Vector(Player(1), Player(2)),
        GameMode.CheatMode,
        None
      )

      game.playSet shouldBe None
    }

    "start in the Action phase" in {
      val game = new Game(
        Vector(Player(1), Player(2)),
        GameMode.StandardDeck,
        None
      )

      game.currentPhase shouldBe TurnPhase.Action
    }

    "advance from Action to Buy phase" in {
      val game = new Game(
        Vector(Player(1), Player(2)),
        GameMode.StandardDeck,
        None
      )

      game.advancePhase()

      game.currentPhase shouldBe TurnPhase.Buy
    }

    "advance from Buy to Cleanup phase" in {
      val game = new Game(
        Vector(Player(1), Player(2)),
        GameMode.StandardDeck,
        None
      )

      game.advancePhase()
      game.advancePhase()

      game.currentPhase shouldBe TurnPhase.Cleanup
    }

    "advance to the next player after Cleanup" in {
      val game = new Game(
        Vector(Player(1), Player(2)),
        GameMode.StandardDeck,
        None
      )

      game.advancePhase()
      game.advancePhase()
      game.advancePhase()

      game.currentPlayer.id shouldBe 2
      game.currentPhase shouldBe TurnPhase.Action
    }

    "return to the first player after the last player" in {
      val game = new Game(
        Vector(Player(1), Player(2)),
        GameMode.StandardDeck,
        None
      )

      game.nextTurn()
      game.currentPlayer.id shouldBe 2

      game.nextTurn()
      game.currentPlayer.id shouldBe 1
    }

    "reset the phase when manually advancing the turn" in {
      val game = new Game(
        Vector(Player(1), Player(2)),
        GameMode.StandardDeck,
        None
      )

      game.advancePhase()
      game.currentPhase shouldBe TurnPhase.Buy

      game.nextTurn()

      game.currentPhase shouldBe TurnPhase.Action
      game.currentPlayer.id shouldBe 2
    }

    "advance through all phases repeatedly" in {
      val game = new Game(
        Vector(Player(1), Player(2)),
        GameMode.StandardDeck,
        None
      )

      game.advancePhase()
      game.currentPhase shouldBe TurnPhase.Buy

      game.advancePhase()
      game.currentPhase shouldBe TurnPhase.Cleanup

      game.advancePhase()
      game.currentPhase shouldBe TurnPhase.Action

      game.currentPlayer.id shouldBe 2

      game.advancePhase()
      game.currentPhase shouldBe TurnPhase.Buy
    }

    "reject a game with fewer than two players" in {
      an[IllegalArgumentException] should be thrownBy {
        new Game(
          Vector(Player(1)),
          GameMode.StandardDeck,
          None
        )
      }
    }

    "reject a game with more than eight players" in {
      val players =
        (1 to 9).map(Player(_)).toVector

      an[IllegalArgumentException] should be thrownBy {
        new Game(
          players,
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
  }
}
