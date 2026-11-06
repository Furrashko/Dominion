
package dominion.model

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class PlayerSpec extends AnyWordSpec with Matchers {

  "A Player" should {

    "have the correct default values" in {
      val player = new Player(1)

      player.id shouldBe 1
      player.perRoundActions shouldBe 1
      player.perRoundMoney shouldBe 0
      player.perRoundBuys shouldBe 1
    }

    "allow custom initial values" in {
      val player = new Player(
        2,
        perRoundActions = 3,
        perRoundMoney = 5,
        perRoundBuys = 2
      )

      player.perRoundActions shouldBe 3
      player.perRoundMoney shouldBe 5
      player.perRoundBuys shouldBe 2
    }

    "start with empty stacks" in {
      val player = new Player(1)

      player.deckSize shouldBe 0
      player.discardPileSize shouldBe 0
    }

    "add a card to the deck" in {
      val player = new Player(1)

      player.addCardToDeck(BasicCards.copper)

      player.deckSize shouldBe 1
    }

    "add multiple cards to the deck" in {
      val player = new Player(1)

      player.addCardsToDeck(
        List(BasicCards.copper, BasicCards.silver)
      )

      player.deckSize shouldBe 2
    }

    "draw a card from a non-empty deck" in {
      val player = new Player(1)

      player.addCardsToDeck(
        List(BasicCards.copper, BasicCards.silver)
      )

      player.drawCard() shouldBe Some(BasicCards.copper)

      player.deckSize shouldBe 1
      player.discardPileSize shouldBe 0
    }

    "return None when both stacks are empty" in {
      val player = new Player(1)

      player.drawCard() shouldBe None
    }

    "discard a card" in {
      val player = new Player(1)

      player.discardCard(BasicCards.copper)

      player.discardPileSize shouldBe 1
    }

    "inspect the discard pile" in {
      val player = new Player(1)

      player.discardCard(BasicCards.copper)
      player.discardCard(BasicCards.silver)

      player.discardPileCards(2) shouldBe
        List(BasicCards.silver, BasicCards.copper)

      player.discardPileSize shouldBe 2
    }

    "take a card from the discard pile" in {
      val player = new Player(1)

      player.discardCard(BasicCards.copper)
      player.discardCard(BasicCards.silver)

      player.takeDiscardCard(0) shouldBe Some(BasicCards.silver)

      player.discardPileSize shouldBe 1
    }

    "return None for an invalid discard pile index" in {
      val player = new Player(1)

      player.discardCard(BasicCards.copper)

      player.takeDiscardCard(-1) shouldBe None
      player.takeDiscardCard(1) shouldBe None
    }

    "shuffle the discard pile into the deck when the deck is empty" in {
      val player = new Player(1)

      player.discardCard(BasicCards.copper)
      player.discardCard(BasicCards.silver)
      player.discardCard(BasicCards.gold)

      player.drawCard().isDefined shouldBe true

      player.deckSize shouldBe 2
      player.discardPileSize shouldBe 0
    }

    "draw all cards after refilling the deck" in {
      val player = new Player(1)

      val cards = List(
        BasicCards.copper,
        BasicCards.silver,
        BasicCards.gold
      )

      cards.foreach(player.discardCard)

      val drawnCards =
        List.fill(3)(player.drawCard().get)

      drawnCards.map(_.name).sorted shouldBe
        List("Copper", "Gold", "Silver")

      player.drawCard() shouldBe None
    }
  }
}
