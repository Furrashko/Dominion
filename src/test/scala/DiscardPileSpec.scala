
package dominion.model

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class DiscardPileSpec extends AnyWordSpec with Matchers {

  "A DiscardPile" should {

    "start empty by default" in {
      val pile = new DiscardPile

      pile.isEmpty shouldBe true
      pile.size shouldBe 0
      pile.peek shouldBe None
      pile.draw() shouldBe None
    }

    "initialize with cards" in {
      val pile = new DiscardPile(
        List(BasicCards.copper, BasicCards.silver)
      )

      pile.size shouldBe 2
      pile.peek shouldBe Some(BasicCards.copper)
    }

    "add a card to the top" in {
      val pile = new DiscardPile(List(BasicCards.copper))

      pile.add(BasicCards.gold)

      pile.peek shouldBe Some(BasicCards.gold)
      pile.size shouldBe 2
    }

    "add multiple cards to the top" in {
      val pile = new DiscardPile(List(BasicCards.gold))

      pile.addAll(
        List(BasicCards.copper, BasicCards.silver)
      )

      pile.draw() shouldBe Some(BasicCards.copper)
      pile.draw() shouldBe Some(BasicCards.silver)
      pile.draw() shouldBe Some(BasicCards.gold)
    }

    "draw the top card" in {
      val pile = new DiscardPile(
        List(BasicCards.copper, BasicCards.silver)
      )

      pile.draw() shouldBe Some(BasicCards.copper)
      pile.size shouldBe 1
    }

    "return None when drawing from an empty pile" in {
      val pile = new DiscardPile

      pile.draw() shouldBe None
    }

    "peek at the top card without removing it" in {
      val pile = new DiscardPile(
        List(BasicCards.copper, BasicCards.silver)
      )

      pile.peek shouldBe Some(BasicCards.copper)
      pile.size shouldBe 2
    }

    "peek at multiple top cards" in {
      val pile = new DiscardPile(
        List(
          BasicCards.copper,
          BasicCards.silver,
          BasicCards.gold
        )
      )

      pile.peekTop(2) shouldBe
        List(BasicCards.copper, BasicCards.silver)

      pile.size shouldBe 3
    }

    "return all available cards when peeking beyond its size" in {
      val pile = new DiscardPile(List(BasicCards.copper))

      pile.peekTop(5) shouldBe List(BasicCards.copper)
    }

    "return an empty list when peeking at zero cards" in {
      val pile = new DiscardPile(List(BasicCards.copper))

      pile.peekTop(0) shouldBe Nil
    }

    "reject negative peek counts" in {
      val pile = new DiscardPile

      an[IllegalArgumentException] should be thrownBy {
        pile.peekTop(-1)
      }
    }

    "take the top cards" in {
      val pile = new DiscardPile(
        List(
          BasicCards.copper,
          BasicCards.silver,
          BasicCards.gold
        )
      )

      pile.takeTop(2) shouldBe
        List(BasicCards.copper, BasicCards.silver)

      pile.size shouldBe 1
      pile.peek shouldBe Some(BasicCards.gold)
    }

    "take all available cards when count exceeds size" in {
      val pile = new DiscardPile(List(BasicCards.copper))

      pile.takeTop(5) shouldBe List(BasicCards.copper)

      pile.isEmpty shouldBe true
    }

    "reject negative take counts" in {
      val pile = new DiscardPile

      an[IllegalArgumentException] should be thrownBy {
        pile.takeTop(-1)
      }
    }

    "take a card at a valid index" in {
      val pile = new DiscardPile(
        List(
          BasicCards.copper,
          BasicCards.silver,
          BasicCards.gold
        )
      )

      pile.takeAt(1) shouldBe Some(BasicCards.silver)

      pile.size shouldBe 2
      pile.peek shouldBe Some(BasicCards.copper)

      pile.draw() shouldBe Some(BasicCards.copper)
      pile.draw() shouldBe Some(BasicCards.gold)
    }

    "take the first card at index zero" in {
      val pile = new DiscardPile(
        List(BasicCards.copper, BasicCards.silver)
      )

      pile.takeAt(0) shouldBe Some(BasicCards.copper)
      pile.peek shouldBe Some(BasicCards.silver)
    }

    "take the last card" in {
      val pile = new DiscardPile(
        List(BasicCards.copper, BasicCards.silver)
      )

      pile.takeAt(1) shouldBe Some(BasicCards.silver)
      pile.size shouldBe 1
    }

    "return None for a negative index" in {
      val pile = new DiscardPile(List(BasicCards.copper))

      pile.takeAt(-1) shouldBe None
    }

    "return None for an index outside the pile" in {
      val pile = new DiscardPile(List(BasicCards.copper))

      pile.takeAt(1) shouldBe None
      pile.takeAt(5) shouldBe None
    }

    "remove all cards" in {
      val pile = new DiscardPile(
        List(BasicCards.copper, BasicCards.silver)
      )

      pile.removeAll() shouldBe
        List(BasicCards.copper, BasicCards.silver)

      pile.isEmpty shouldBe true
      pile.size shouldBe 0
    }

    "return an empty list when removing all cards from an empty pile" in {
      val pile = new DiscardPile

      pile.removeAll() shouldBe Nil
      pile.isEmpty shouldBe true
    }
  }
}
