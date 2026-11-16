package dominion.model

import dominion.model.effects.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ActionCardEffectSpec extends AnyWordSpec with Matchers {

  "Action card effects" should {

    "draw cards into the player's hand" in {
      val player = new Player(1)
      player.addCardsToDeck(List(BasicCards.copper, BasicCards.silver))

      DrawCardsEffect(2).execute(player)

      player.handSize shouldBe 2
      player.handCards shouldBe List(BasicCards.copper, BasicCards.silver)
      player.deckSize shouldBe 0
    }

    "add actions, money and buys" in {
      val player = new Player(1)

      AddActionsEffect(2).execute(player)
      AddMoneyEffect(3).execute(player)
      AddBuysEffect(1).execute(player)

      player.perRoundActions shouldBe 3
      player.perRoundMoney shouldBe 3
      player.perRoundBuys shouldBe 2
    }

    "execute all effects of a card in order" in {
      val player = new Player(1)
      player.addCardToDeck(BasicCards.copper)

      new ActionCards.Village().play(player)

      player.handCards shouldBe List(BasicCards.copper)
      player.perRoundActions shouldBe 3
    }

    "not draw more cards than are available" in {
      val player = new Player(1)
      player.addCardToDeck(BasicCards.copper)

      DrawCardsEffect(3).execute(player)

      player.handCards shouldBe List(BasicCards.copper)
      player.handSize shouldBe 1
    }

    "reject negative effect amounts" in {
      an[IllegalArgumentException] should be thrownBy DrawCardsEffect(-1)
      an[IllegalArgumentException] should be thrownBy AddActionsEffect(-1)
      an[IllegalArgumentException] should be thrownBy AddMoneyEffect(-1)
      an[IllegalArgumentException] should be thrownBy AddBuysEffect(-1)
    }
  }
}
