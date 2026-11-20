
package dominion.model

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CardFactorySpec extends AnyWordSpec with Matchers {

  "CardFactory" should {

    "create an Estate card" in {
      val card = CardFactory("Estate")

      card shouldBe a[VictoryCard]
      card.name shouldBe "Estate"
      card.cost shouldBe 2
    }

    "create a Duchy card" in {
      val card = CardFactory("Duchy")

      card shouldBe a[VictoryCard]
      card.name shouldBe "Duchy"
      card.cost shouldBe 5
    }

    "create a Province card" in {
      val card = CardFactory("Province")

      card shouldBe a[VictoryCard]
      card.name shouldBe "Province"
      card.cost shouldBe 8
    }

    "create a Copper card" in {
      val card = CardFactory("Copper")

      card shouldBe a[TreasureCard]
      card.name shouldBe "Copper"
      card.cost shouldBe 0
    }

    "create all supported cards" in {
      CardFactory.availableNames should contain allOf(
        "Estate",
        "Duchy",
        "Province",
        "Copper",
        "Silver",
        "Gold"
      )
    }

    "return None for an unknown card" in {
      CardFactory.create("Unknown") shouldBe None
    }

    "throw an exception for an unknown card using apply" in {
      an[IllegalArgumentException] should be thrownBy {
        CardFactory("Unknown")
      }
    }

    "create multiple cards" in {
      val cards = CardFactory.createAll(
        List("Copper", "Silver", "Gold")
      )

      cards shouldBe defined
      cards.get.map(_.name) shouldBe
        Vector("Copper", "Silver", "Gold")
    }

    "return None when one card cannot be created" in {
      CardFactory.createAll(
        List("Copper", "Unknown", "Gold")
      ) shouldBe None
    }

    "create independent card instances" in {
      val first = CardFactory("Copper")
      val second = CardFactory("Copper")

      (first eq second) shouldBe false
    }
  }
}
