
package dominion.model

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class CardSpec extends AnyWordSpec with Matchers {

  private class TestActionCard(name: String, cost: Int)
    extends ActionCard(name, cost)

  "A Card" should {

    "have a name and cost" in {
      val card = new TestActionCard("Village", 3)

      card.name shouldBe "Village"
      card.cost shouldBe 3
    }

    "not allow an empty name" in {
      an[IllegalArgumentException] should be thrownBy {
        new TestActionCard("", 3)
      }
    }

    "not allow a negative cost" in {
      an[IllegalArgumentException] should be thrownBy {
        new TestActionCard("Village", -1)
      }
    }
  }

  "A VictoryCard" should {

    "have the correct victory points" in {
      BasicCards.estate.victoryPoints shouldBe 1
      BasicCards.duchy.victoryPoints shouldBe 3
      BasicCards.province.victoryPoints shouldBe 6
    }

    "have the correct costs" in {
      BasicCards.estate.cost shouldBe 2
      BasicCards.duchy.cost shouldBe 5
      BasicCards.province.cost shouldBe 8
    }

    "not allow negative victory points" in {
      an[IllegalArgumentException] should be thrownBy {
        new VictoryCard("Invalid", 2, -1)
      }
    }

    "allow zero victory points" in {
      val card = new VictoryCard("Test", 0, 0)

      card.victoryPoints shouldBe 0
    }
  }

  "A TreasureCard" should {

    "have the correct values and costs" in {
      BasicCards.copper.value shouldBe 1
      BasicCards.copper.cost shouldBe 0

      BasicCards.silver.value shouldBe 2
      BasicCards.silver.cost shouldBe 3

      BasicCards.gold.value shouldBe 3
      BasicCards.gold.cost shouldBe 6
    }

    "not allow negative treasure values" in {
      an[IllegalArgumentException] should be thrownBy {
        new TreasureCard("Invalid", 1, -1)
      }
    }

    "allow zero treasure value" in {
      val card = new TreasureCard("Test", 0, 0)

      card.value shouldBe 0
    }
  }

  "An ActionCard" should {

    "have a name and cost" in {
      val card = new TestActionCard("Smithy", 4)

      card.name shouldBe "Smithy"
      card.cost shouldBe 4
    }

    "support a cost of zero" in {
      val card = new TestActionCard("Free Action", 0)

      card.cost shouldBe 0
    }
  }

  "BasicCards" should {

    "provide all basic cards" in {
      BasicCards.estate shouldBe a[VictoryCard]
      BasicCards.duchy shouldBe a[VictoryCard]
      BasicCards.province shouldBe a[VictoryCard]

      BasicCards.copper shouldBe a[TreasureCard]
      BasicCards.silver shouldBe a[TreasureCard]
      BasicCards.gold shouldBe a[TreasureCard]
    }
  }
}
