
package dominion.model

import dominion.model.playset.PlaySetDefinition
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
      game.players.map(_.id) shouldBe (1 to 8).toVector
      game.gameMode shouldBe GameMode.CheatMode
      game.playSet shouldBe Some(PlaySet.FirstGame)
    }

    "create independent players" in {
      val game = GameFactory.create(
        2,
        GameMode.StandardDeck,
        None
      )

      (game.players(0) eq game.players(1)) shouldBe false
    }

    "deal five cards to every player" in {
      val game = GameFactory.create(
        4,
        GameMode.StandardDeck,
        Some(PlaySet.FirstGame)
      )

      game.players.map(_.handSize) shouldBe Vector.fill(4)(5)
    }

    "leave five cards in every player's draw pile" in {
      val game = GameFactory.create(
        4,
        GameMode.StandardDeck,
        Some(PlaySet.FirstGame)
      )

      game.players.map(_.deckSize) shouldBe Vector.fill(4)(5)
    }

    "give every player three starting victory points" in {
      val game = GameFactory.create(
        4,
        GameMode.StandardDeck,
        Some(PlaySet.FirstGame)
      )

      game.players.foreach { player =>
        player.victoryPoints shouldBe 3
      }
    }

    "create a separate play table for every game" in {
      val first = GameFactory.create(
        2,
        GameMode.StandardDeck,
        Some(PlaySet.FirstGame)
      )

      val second = GameFactory.create(
        2,
        GameMode.StandardDeck,
        Some(PlaySet.FirstGame)
      )

      (first.playTable eq second.playTable) shouldBe false
    }

    "create fresh action card instances for different games" in {
      val first = GameFactory.create(
        2,
        GameMode.StandardDeck,
        Some(PlaySet.FirstGame)
      )

      val second = GameFactory.create(
        2,
        GameMode.StandardDeck,
        Some(PlaySet.FirstGame)
      )

      val firstSmithy = first.playTable.getPile("Smithy").get.card
      val secondSmithy = second.playTable.getPile("Smithy").get.card

      (firstSmithy eq secondSmithy) shouldBe false
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

    "create a game using a custom definition" in {
      val definition = PlaySetDefinition.custom(
        "Custom",
        (0 until 10).map { index =>
          () => new ActionCard(s"Custom $index", 2) {}
        }
      )

      val game = GameFactory.createWithDefinition(
        2,
        GameMode.StandardDeck,
        None,
        definition
      )

      game.players should have size 2
      game.playTable.getPile("Custom 0") shouldBe defined
    }

    "create a game with a custom definition and preserve its PlaySet option" in {
      val definition = PlaySetDefinition.custom(
        "Custom",
        (0 until 10).map { index =>
          () => new ActionCard(s"Custom $index", 2) {}
        }
      )

      val game = GameFactory.createWithDefinition(
        3,
        GameMode.CheatMode,
        Some(PlaySet.FirstGame),
        definition
      )

      game.gameMode shouldBe GameMode.CheatMode
      game.playSet shouldBe Some(PlaySet.FirstGame)
      game.players should have size 3
    }
  }
}
