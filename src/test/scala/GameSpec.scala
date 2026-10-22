import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class GameSpec extends AnyWordSpec with Matchers {

  "A Game" should {

    "create the requested number of players" in {
      val game = new Game(4)

      game.players should have size 4
    }

    "assign player IDs starting at 1" in {
      val game = new Game(4)

      game.players.map(_.id) shouldBe Vector(1, 2, 3, 4)
    }

    "start with the first player" in {
      val game = new Game(4)

      game.currentPlayer.id shouldBe 1
    }

    "advance to the next player" in {
      val game = new Game(4)

      game.nextTurn()

      game.currentPlayer.id shouldBe 2
    }

    "return to the first player after the last player" in {
      val game = new Game(4)

      (1 to 4).foreach(_ => game.nextTurn())

      game.currentPlayer.id shouldBe 1
    }

    "reject fewer than two players" in {
      an[IllegalArgumentException] should be thrownBy {
        new Game(1)
      }
    }

    "reject more than eight players" in {
      an[IllegalArgumentException] should be thrownBy {
        new Game(9)
      }
    }
  }
}