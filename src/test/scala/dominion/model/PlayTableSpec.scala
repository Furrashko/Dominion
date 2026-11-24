
package dominion.model

import dominion.model.playset.PlaySetDefinition
import dominion.model.table.{CardPile, PlayTable, PlayTableFactory}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PlayTableSpec extends AnyWordSpec with Matchers {

  private def definition: PlaySetDefinition =
    PlaySetDefinition.custom(
      "Test Set",
      (0 until 10).map { index =>
        () => new ActionCard(s"Action $index", 2) {}
      }
    )

  "CardPile" should {

    "initialize with the specified amount" in {
      val pile = new CardPile(BasicCards.copper, 10)

      pile.count shouldBe 10
      pile.isEmpty shouldBe false
    }

    "initialize as empty" in {
      val pile = new CardPile(BasicCards.copper, 0)

      pile.count shouldBe 0
      pile.isEmpty shouldBe true
    }

    "reject a negative initial amount" in {
      an[IllegalArgumentException] should be thrownBy {
        new CardPile(BasicCards.copper, -1)
      }
    }

    "return the card when taking from a non-empty pile" in {
      val pile = new CardPile(BasicCards.copper, 2)

      pile.take() shouldBe Some(BasicCards.copper)
      pile.count shouldBe 1
    }

    "return None when taking from an empty pile" in {
      val pile = new CardPile(BasicCards.copper, 0)

      pile.take() shouldBe None
      pile.count shouldBe 0
    }

    "become empty after taking the last card" in {
      val pile = new CardPile(BasicCards.copper, 1)

      pile.take()

      pile.isEmpty shouldBe true
    }

    "restore its amount" in {
      val pile = new CardPile(BasicCards.copper, 5)

      pile.take()
      pile.restore(5)

      pile.count shouldBe 5
    }

    "reject a negative restored amount" in {
      val pile = new CardPile(BasicCards.copper, 5)

      an[IllegalArgumentException] should be thrownBy {
        pile.restore(-1)
      }
    }
  }

  "PlayTable" should {

    "reject duplicate pile names" in {
      an[IllegalArgumentException] should be thrownBy {
        new PlayTable(
          List(
            (BasicCards.copper, 10),
            (BasicCards.copper, 20)
          )
        )
      }
    }

    "return all piles sorted by card name" in {
      val table = new PlayTable(
        List(
          (BasicCards.gold, 10),
          (BasicCards.copper, 10),
          (BasicCards.silver, 10)
        )
      )

      table.allPiles.map(_.card.name) shouldBe
        List("Copper", "Gold", "Silver")
    }

    "find an existing pile" in {
      val table = new PlayTable(
        List((BasicCards.copper, 10))
      )

      table.getPile("Copper").map(_.card) shouldBe
        Some(BasicCards.copper)
    }

    "return None for an unknown pile" in {
      val table = new PlayTable(
        List((BasicCards.copper, 10))
      )

      table.getPile("Unknown") shouldBe None
    }

    "return all available cards" in {
      val table = new PlayTable(
        List(
          (BasicCards.copper, 10),
          (BasicCards.silver, 0)
        )
      )

      table.availableCards shouldBe List(BasicCards.copper)
    }

    "buy a card from an existing pile" in {
      val table = new PlayTable(
        List((BasicCards.copper, 2))
      )

      table.buyCard("Copper") shouldBe Some(BasicCards.copper)
      table.getPile("Copper").get.count shouldBe 1
    }

    "return None when buying from an empty pile" in {
      val table = new PlayTable(
        List((BasicCards.copper, 0))
      )

      table.buyCard("Copper") shouldBe None
    }

    "return None when buying from an unknown pile" in {
      val table = new PlayTable(
        List((BasicCards.copper, 2))
      )

      table.buyCard("Unknown") shouldBe None
    }

    "identify an empty pile" in {
      val table = new PlayTable(
        List((BasicCards.copper, 0))
      )

      table.isPileEmpty("Copper") shouldBe true
    }

    "identify a non-empty pile" in {
      val table = new PlayTable(
        List((BasicCards.copper, 1))
      )

      table.isPileEmpty("Copper") shouldBe false
    }

    "treat an unknown pile as empty" in {
      val table = new PlayTable(
        List((BasicCards.copper, 1))
      )

      table.isPileEmpty("Unknown") shouldBe true
    }

    "count empty piles" in {
      val table = new PlayTable(
        List(
          (BasicCards.copper, 0),
          (BasicCards.silver, 1),
          (BasicCards.gold, 0)
        )
      )

      table.emptyPileCount shouldBe 2
    }

    "end the game when Province is empty" in {
      val table = new PlayTable(
        List(
          (BasicCards.province, 0),
          (BasicCards.copper, 10)
        )
      )

      table.isGameOver shouldBe true
    }

    "end the game when three piles are empty" in {
      val table = new PlayTable(
        List(
          (BasicCards.province, 1),
          (BasicCards.copper, 0),
          (BasicCards.silver, 0),
          (BasicCards.gold, 0)
        )
      )

      table.isGameOver shouldBe true
    }

    "not end the game when fewer than three piles are empty" in {
      val table = new PlayTable(
        List(
          (BasicCards.province, 1),
          (BasicCards.copper, 0),
          (BasicCards.silver, 0),
          (BasicCards.gold, 1)
        )
      )

      table.isGameOver shouldBe false
    }

    "restore a snapshot" in {
      val table = new PlayTable(
        List(
          (BasicCards.copper, 5),
          (BasicCards.silver, 3)
        )
      )

      val snapshot = table.snapshot

      table.buyCard("Copper")
      table.buyCard("Silver")

      table.restore(snapshot)

      table.getPile("Copper").get.count shouldBe 5
      table.getPile("Silver").get.count shouldBe 3
    }

    "reject a snapshot with different pile names" in {
      val table = new PlayTable(
        List((BasicCards.copper, 5))
      )

      an[IllegalArgumentException] should be thrownBy {
        table.restore(Map("Silver" -> 5))
      }
    }
  }

  "PlayTableFactory" should {

    "create a table for two players" in {
      val table = PlayTableFactory.create(2, definition)

      table.getPile("Estate").get.count shouldBe 8
      table.getPile("Duchy").get.count shouldBe 8
      table.getPile("Province").get.count shouldBe 8
      table.getPile("Copper").get.count shouldBe 46
    }

    "create a table for more than two players" in {
      val table = PlayTableFactory.create(4, definition)

      table.getPile("Estate").get.count shouldBe 12
      table.getPile("Duchy").get.count shouldBe 12
      table.getPile("Province").get.count shouldBe 12
      table.getPile("Copper").get.count shouldBe 32
    }

    "create ten action card piles" in {
      val table = PlayTableFactory.create(2, definition)

      table.allPiles.count(_.card.isInstanceOf[ActionCard]) shouldBe 10
    }

    "initialize every action pile with ten cards" in {
      val table = PlayTableFactory.create(2, definition)

      table.allPiles
        .filter(_.card.isInstanceOf[ActionCard])
        .foreach(_.count shouldBe 10)
    }

    "reject fewer than two players" in {
      an[IllegalArgumentException] should be thrownBy {
        PlayTableFactory.create(1, definition)
      }
    }

    "reject more than eight players" in {
      an[IllegalArgumentException] should be thrownBy {
        PlayTableFactory.create(9, definition)
      }
    }
  }
}
