
package dominion.model

import dominion.model.table.PlayTableFactory
import dominion.util.PlaySet
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ActionContextSpec extends AnyWordSpec with Matchers {

  private def createContext(
                             decisions: DecisionProvider = DecisionProvider.default
                           ): ActionContext = {
    val game = GameFactory.create(
      2,
      GameMode.StandardDeck,
      Some(PlaySet.FirstGame)
    )

    new ActionContext(
      game,
      game.currentPlayer,
      game.playTable,
      decisions
    )
  }

  "DecisionProvider" should {

    "select the first candidates by default for discard" in {
      val provider = DecisionProvider.default
      val player = new Player(1)
      val cards = List(BasicCards.copper, BasicCards.silver)

      provider.chooseCardsToDiscard(player, cards, 1) shouldBe
        List(BasicCards.copper)
    }

    "return no Cellar discards by default" in {
      DecisionProvider.default.chooseCellarDiscards(
        new Player(1),
        List(BasicCards.copper)
      ) shouldBe empty
    }

    "select the first card to trash by default" in {
      DecisionProvider.default.chooseCardToTrash(
        new Player(1),
        List(BasicCards.copper)
      ) shouldBe Some(BasicCards.copper)
    }

    "return None when no trash candidates exist" in {
      DecisionProvider.default.chooseCardToTrash(
        new Player(1),
        Nil
      ) shouldBe None
    }

    "select the most expensive affordable card to gain" in {
      val cards = List(
        BasicCards.copper,
        BasicCards.silver,
        BasicCards.gold
      )

      DecisionProvider.default.chooseCardToGain(
        new Player(1),
        cards,
        3
      ) shouldBe Some(BasicCards.silver)
    }

    "return None when no card is affordable" in {
      DecisionProvider.default.chooseCardToGain(
        new Player(1),
        List(BasicCards.gold),
        2
      ) shouldBe None
    }

    "reveal Moat when the player has one" in {
      val player = new Player(2)
      player.addCardToHand(
        new dominion.model.actioncard.Moat
      )

      DecisionProvider.default.shouldRevealMoat(player) shouldBe true
    }

    "not reveal Moat when the player has none" in {
      DecisionProvider.default.shouldRevealMoat(
        new Player(2)
      ) shouldBe false
    }
  }

  "ActionContext" should {

    "expose its game, player, table and decisions" in {
      val context = createContext()

      context.game should not be null
      context.player shouldBe context.game.currentPlayer
      context.table shouldBe context.game.playTable
      context.decisions shouldBe DecisionProvider.default
    }

    "discard selected cards from a player's hand" in {
      val context = createContext()
      val player = context.player
      val card = BasicCards.copper

      player.addCardToHand(card)

      context.discardFromHand(player, List(card))

      player.handCards should not contain card
      player.discardPileSize shouldBe 1
    }

    "discard multiple selected cards" in {
      val context = createContext()
      val player = context.player

      val cards = List(
        BasicCards.copper,
        BasicCards.silver
      )

      cards.foreach(player.addCardToHand)

      context.discardFromHand(player, cards)

      player.handCards shouldBe empty
      player.discardPileSize shouldBe 2
    }

    "allow discarding an empty selection" in {
      val context = createContext()

      context.discardFromHand(context.player, Nil)

      context.player.discardPileSize shouldBe 0
    }

    "reject duplicate selected cards" in {
      val context = createContext()
      val card = BasicCards.copper

      context.player.addCardToHand(card)

      an[IllegalArgumentException] should be thrownBy {
        context.discardFromHand(
          context.player,
          List(card, card)
        )
      }
    }

    "reject cards that are not in the hand" in {
      val context = createContext()

      an[IllegalArgumentException] should be thrownBy {
        context.discardFromHand(
          context.player,
          List(BasicCards.copper)
        )
      }
    }

    "trash a card from the hand" in {
      val context = createContext()
      val card = BasicCards.copper

      context.player.addCardToHand(card)
      context.trashFromHand(context.player, card)

      context.player.handCards shouldBe empty
      context.player.discardPileSize shouldBe 0
    }

    "reject trashing a card that is not in the hand" in {
      val context = createContext()

      an[IllegalArgumentException] should be thrownBy {
        context.trashFromHand(
          context.player,
          BasicCards.copper
        )
      }
    }

    "gain a card to the discard pile" in {
      val context = createContext()

      val gained = context.gainToDiscard(BasicCards.silver)

      gained shouldBe Some(BasicCards.silver)
      context.player.discardPileSize shouldBe 1
    }

    "gain a card to the hand" in {
      val context = createContext()

      val gained = context.gainToHand(BasicCards.silver)

      gained shouldBe Some(BasicCards.silver)
      context.player.handCards should contain(BasicCards.silver)
    }

    "return None when a card is not available in the supply" in {
      val context = createContext()

      context.gainToHand(
        new TreasureCard("Unknown Treasure", 0, 1)
      ) shouldBe None
    }
  }
}
