
package dominion.model

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class DeckSpec extends AnyWordSpec with Matchers {

  "A Deck" should {

    "start empty by default" in {
      val deck = new Deck

      deck.isEmpty shouldBe true
      deck.size shouldBe 0
      deck.peek shouldBe None
      deck.draw() shouldBe None
    }

    "initialize with cards" in {
      val deck = new Deck(
        List(BasicCards.copper, BasicCards.silver)
      )

      deck.isEmpty shouldBe false
      deck.size shouldBe 2
      deck.peek shouldBe Some(BasicCards.copper)
    }

    "draw the top card" in {
      val deck = new Deck(
        List(BasicCards.copper, BasicCards.silver)
      )

      deck.draw() shouldBe Some(BasicCards.copper)
      deck.size shouldBe 1
      deck.peek shouldBe Some(BasicCards.silver)
    }

    "return None when drawing from an empty deck" in {
      val deck = new Deck

      deck.draw() shouldBe None
    }

    "add a card to the top" in {
      val deck = new Deck(List(BasicCards.copper))

      deck.addToTop(BasicCards.gold)

      deck.peek shouldBe Some(BasicCards.gold)
      deck.size shouldBe 2
    }

    "add a card to the bottom" in {
      val deck = new Deck(List(BasicCards.copper))

      deck.addToBottom(BasicCards.gold)

      deck.draw() shouldBe Some(BasicCards.copper)
      deck.draw() shouldBe Some(BasicCards.gold)
    }

    "add multiple cards to the top" in {
      val deck = new Deck(List(BasicCards.gold))

      deck.addAllToTop(
        List(BasicCards.copper, BasicCards.silver)
      )

      deck.draw() shouldBe Some(BasicCards.copper)
      deck.draw() shouldBe Some(BasicCards.silver)
      deck.draw() shouldBe Some(BasicCards.gold)
    }

    "shuffle cards without losing or duplicating them" in {
      val deck = new Deck(
        List(
          BasicCards.copper,
          BasicCards.silver,
          BasicCards.gold
        )
      )

      deck.shuffle()

      deck.size shouldBe 3

      val drawnCards =
        List.fill(3)(deck.draw().get)

      drawnCards.map(_.name).sorted shouldBe
        List("Copper", "Gold", "Silver")
    }

    "clear all cards" in {
      val deck = new Deck(List(BasicCards.copper))

      deck.clear()

      deck.isEmpty shouldBe true
      deck.size shouldBe 0
      deck.peek shouldBe None
      deck.draw() shouldBe None
    }
  }
}
