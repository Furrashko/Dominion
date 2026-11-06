
package dominion.model

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

    "advance to the next player" in {
      val game = new Game(
        Vector(Player(1), Player(2), Player(3)),
        GameMode.StandardDeck,
        Some(PlaySet.FirstGame)
      )

      game.nextTurn()
      game.currentPlayer.id shouldBe 2

      game.nextTurn()
      game.currentPlayer.id shouldBe 3
    }

    "return to the first player after the last player" in {
      val game = new Game(
        Vector(Player(1), Player(2)),
        GameMode.StandardDeck,
        Some(PlaySet.FirstGame)
      )

      game.nextTurn()
      game.currentPlayer.id shouldBe 2

      game.nextTurn()
      game.currentPlayer.id shouldBe 1
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
