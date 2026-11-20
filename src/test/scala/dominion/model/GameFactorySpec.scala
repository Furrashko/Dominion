
package dominion.model

import dominion.util.PlaySet
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GameFactorySpec extends AnyWordSpec with Matchers {

  "GameFactory" should {

    "create a game with two players" in {

      val game = GameFactory.create(
        2,
        GameMode.StandardDeck,
        Some(PlaySet.FirstGame)
      )

      game.players should have size 2

      game.players.map(_.id) shouldBe Vector(1, 2)

      game.gameMode shouldBe GameMode.StandardDeck

      game.playSet shouldBe Some(PlaySet.FirstGame)
    }

    "create a game with eight players" in {

      val game = GameFactory.create(
        8,
        GameMode.CheatMode,
        None
      )

      game.players should have size 8

      game.players.map(_.id) shouldBe
        (1 to 8).toVector
    }

    "create independent players" in {

      val game = GameFactory.create(
        2,
        GameMode.StandardDeck,
        None
      )

      (game.players(0) eq game.players(1)) shouldBe false
    }

    "reject fewer than two players" in {

      an[IllegalArgumentException] should be thrownBy {
        GameFactory.create(
          1,
          GameMode.StandardDeck,
          None
        )
      }
    }

    "reject more than eight players" in {

      an[IllegalArgumentException] should be thrownBy {
        GameFactory.create(
          9,
          GameMode.StandardDeck,
          None
        )
      }
    }
  }
}
