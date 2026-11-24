
package dominion.model

import dominion.model.actioncard.*
import dominion.model.playset.PlaySetCatalog
import dominion.util.PlaySet
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ActionCardsSpec extends AnyWordSpec with Matchers {

  private def context(
                       player: Player,
                       opponent: Player,
                       decisions: DecisionProvider = DecisionProvider.default
                     ): ActionContext = {
    val game = new Game(
      Vector(player, opponent),
      GameMode.StandardDeck,
      Some(PlaySet.FirstGame)
    )

    new ActionContext(
      game,
      player,
      game.playTable,
      decisions
    )
  }

  "Cellar" should {

    "discard selected cards and draw the same amount" in {
      val player = new Player(1)
      val opponent = new Player(2)

      val copper = BasicCards.copper
      player.addCardToHand(copper)
      player.addCardToDeck(BasicCards.silver)

      val decisions = new DecisionProvider {
        override def chooseCellarDiscards(
                                           player: Player,
                                           candidates: List[Card]
                                         ): List[Card] = candidates
      }

      val actionContext = context(player, opponent, decisions)

      new Cellar().play(actionContext)

      player.discardPileSize shouldBe 1
      player.handCards should contain(BasicCards.silver)
      player.availableActions shouldBe 2
    }

    "allow playing without discarding cards" in {
      val player = new Player(1)
      val opponent = new Player(2)

      val actionContext = context(player, opponent)

      new Cellar().play(actionContext)

      player.discardPileSize shouldBe 0
      player.availableActions shouldBe 2
    }

    "reject an invalid selection" in {
      val player = new Player(1)
      val opponent = new Player(2)

      player.addCardToHand(BasicCards.copper)

      val decisions = new DecisionProvider {
        override def chooseCellarDiscards(
                                           player: Player,
                                           candidates: List[Card]
                                         ): List[Card] =
          List(BasicCards.silver)
      }

      val actionContext = context(player, opponent, decisions)

      an[IllegalArgumentException] should be thrownBy {
        new Cellar().play(actionContext)
      }
    }
  }

  "Militia" should {

    "add two money and make opponents discard down to three cards" in {
      val attacker = new Player(1)
      val opponent = new Player(2)

      opponent.addCardsToDeck(
        List.fill(5)(BasicCards.copper)
      )
      opponent.drawCardsToHand(5)

      val actionContext = context(attacker, opponent)

      new Militia().play(actionContext)

      attacker.perRoundMoney shouldBe 2
      opponent.handSize shouldBe 3
      opponent.discardPileSize shouldBe 2
    }

    "not force opponents with three or fewer cards to discard" in {
      val attacker = new Player(1)
      val opponent = new Player(2)

      opponent.addCardsToDeck(
        List.fill(3)(BasicCards.copper)
      )
      opponent.drawCardsToHand(3)

      new Militia().play(context(attacker, opponent))

      opponent.handSize shouldBe 3
      opponent.discardPileSize shouldBe 0
    }

    "not force opponents with Moat to discard when the attack is blocked" in {
      val attacker = new Player(1)
      val opponent = new Player(2)

      opponent.addCardsToDeck(
        List.fill(4)(BasicCards.copper)
      )
      opponent.drawCardsToHand(4)
      opponent.addCardToHand(new Moat)

      val decisions = new DecisionProvider {
        override def shouldRevealMoat(
                                       defendingPlayer: Player
                                     ): Boolean = true
      }

      new Militia().play(context(attacker, opponent, decisions))

      opponent.handSize shouldBe 5
      opponent.discardPileSize shouldBe 0
    }

    "allow opponents to decline revealing Moat" in {
      val attacker = new Player(1)
      val opponent = new Player(2)

      opponent.addCardsToDeck(
        List.fill(4)(BasicCards.copper)
      )
      opponent.drawCardsToHand(4)
      opponent.addCardToHand(new Moat)

      val decisions = new DecisionProvider {
        override def shouldRevealMoat(
                                       defendingPlayer: Player
                                     ): Boolean = false
      }

      new Militia().play(context(attacker, opponent, decisions))

      opponent.handSize shouldBe 3
      opponent.discardPileSize shouldBe 2
    }

    "reject an invalid discard selection" in {
      val attacker = new Player(1)
      val opponent = new Player(2)

      opponent.addCardsToDeck(
        List.fill(5)(BasicCards.copper)
      )
      opponent.drawCardsToHand(5)

      val decisions = new DecisionProvider {
        override def chooseCardsToDiscard(
                                           player: Player,
                                           candidates: List[Card],
                                           count: Int
                                         ): List[Card] = Nil
      }

      an[IllegalArgumentException] should be thrownBy {
        new Militia().play(context(attacker, opponent, decisions))
      }
    }
  }

  "Mine" should {

    "trash a treasure and gain a more expensive treasure to hand" in {
      val player = new Player(1)
      val opponent = new Player(2)

      player.addCardToHand(BasicCards.copper)

      val decisions = new DecisionProvider {
        override def chooseCardToTrash(
                                        player: Player,
                                        candidates: List[Card]
                                      ): Option[Card] = candidates.headOption

        override def chooseCardToGain(
                                       player: Player,
                                       candidates: List[Card],
                                       maxCost: Int
                                     ): Option[Card] =
          candidates.find(_.name == "Silver")
      }

      val actionContext = context(player, opponent, decisions)

      new Mine().play(actionContext)

      player.handCards should contain(BasicCards.silver)
      player.handCards should not contain BasicCards.copper
      player.discardPileSize shouldBe 0
    }

    "do nothing when no treasure can be trashed" in {
      val player = new Player(1)
      val opponent = new Player(2)

      player.addCardToHand(new ActionCard("Action", 2) {})

      val actionContext = context(player, opponent)

      new Mine().play(actionContext)

      player.handCards should have size 1
    }

    "reject selecting a non-treasure card for trashing" in {
      val player = new Player(1)
      val opponent = new Player(2)
      val action = new ActionCard("Action", 2) {}

      player.addCardToHand(action)

      val decisions = new DecisionProvider {
        override def chooseCardToTrash(
                                        player: Player,
                                        candidates: List[Card]
                                      ): Option[Card] = Some(action)
      }

      an[IllegalArgumentException] should be thrownBy {
        new Mine().play(context(player, opponent, decisions))
      }
    }

    "not gain a card when the selected gain is invalid" in {
      val player = new Player(1)
      val opponent = new Player(2)

      player.addCardToHand(BasicCards.copper)

      val decisions = new DecisionProvider {
        override def chooseCardToTrash(
                                        player: Player,
                                        candidates: List[Card]
                                      ): Option[Card] = candidates.headOption

        override def chooseCardToGain(
                                       player: Player,
                                       candidates: List[Card],
                                       maxCost: Int
                                     ): Option[Card] =
          Some(new TreasureCard("Foreign", 0, 1))
      }

      new Mine().play(context(player, opponent, decisions))

      player.handCards should not contain BasicCards.copper
      player.discardPileSize shouldBe 0
    }
  }

  "Moat" should {

    "draw two cards" in {
      val player = new Player(1)
      val opponent = new Player(2)

      player.addCardsToDeck(
        List(BasicCards.copper, BasicCards.silver)
      )

      new Moat().play(context(player, opponent))

      player.handCards shouldBe
        List(BasicCards.copper, BasicCards.silver)
    }

    "draw fewer than two cards when the deck is insufficient" in {
      val player = new Player(1)
      val opponent = new Player(2)

      player.addCardToDeck(BasicCards.copper)

      new Moat().play(context(player, opponent))

      player.handSize shouldBe 1
    }
  }

  "Remodel" should {

    "trash a card and gain a card costing at most two more" in {
      val player = new Player(1)
      val opponent = new Player(2)

      player.addCardToHand(BasicCards.copper)

      val decisions = new DecisionProvider {
        override def chooseCardToTrash(
                                        player: Player,
                                        candidates: List[Card]
                                      ): Option[Card] = candidates.headOption

        override def chooseCardToGain(
                                       player: Player,
                                       candidates: List[Card],
                                       maxCost: Int
                                     ): Option[Card] =
          candidates.find(_.name == "Estate")
      }

      new Remodel().play(context(player, opponent, decisions))

      player.handCards shouldBe empty
      player.discardPileSize shouldBe 1
    }

    "do nothing when no card is selected for trashing" in {
      val player = new Player(1)
      val opponent = new Player(2)

      player.addCardToHand(BasicCards.copper)

      val decisions = new DecisionProvider {
        override def chooseCardToTrash(
                                        player: Player,
                                        candidates: List[Card]
                                      ): Option[Card] = None
      }

      new Remodel().play(context(player, opponent, decisions))

      player.handCards should contain(BasicCards.copper)
      player.discardPileSize shouldBe 0
    }

    "reject a card that is not in the hand" in {
      val player = new Player(1)
      val opponent = new Player(2)

      val decisions = new DecisionProvider {
        override def chooseCardToTrash(
                                        player: Player,
                                        candidates: List[Card]
                                      ): Option[Card] =
          Some(BasicCards.copper)
      }

      an[IllegalArgumentException] should be thrownBy {
        new Remodel().play(context(player, opponent, decisions))
      }
    }

    "not gain a card when the selected card is invalid" in {
      val player = new Player(1)
      val opponent = new Player(2)

      player.addCardToHand(BasicCards.copper)

      val decisions = new DecisionProvider {
        override def chooseCardToTrash(
                                        player: Player,
                                        candidates: List[Card]
                                      ): Option[Card] = candidates.headOption

        override def chooseCardToGain(
                                       player: Player,
                                       candidates: List[Card],
                                       maxCost: Int
                                     ): Option[Card] =
          Some(new TreasureCard("Foreign", 0, 1))
      }

      new Remodel().play(context(player, opponent, decisions))

      player.discardPileSize shouldBe 0
    }
  }

  "Workshop" should {

    "gain a card costing at most four to the discard pile" in {
      val player = new Player(1)
      val opponent = new Player(2)

      val decisions = new DecisionProvider {
        override def chooseCardToGain(
                                       player: Player,
                                       candidates: List[Card],
                                       maxCost: Int
                                     ): Option[Card] =
          candidates.find(_.name == "Silver")
      }

      new Workshop().play(context(player, opponent, decisions))

      player.discardPileSize shouldBe 1
    }

    "do nothing when no card is selected" in {
      val player = new Player(1)
      val opponent = new Player(2)

      val decisions = new DecisionProvider {
        override def chooseCardToGain(
                                       player: Player,
                                       candidates: List[Card],
                                       maxCost: Int
                                     ): Option[Card] = None
      }

      new Workshop().play(context(player, opponent, decisions))

      player.discardPileSize shouldBe 0
    }

    "ignore an invalid selected card" in {
      val player = new Player(1)
      val opponent = new Player(2)

      val decisions = new DecisionProvider {
        override def chooseCardToGain(
                                       player: Player,
                                       candidates: List[Card],
                                       maxCost: Int
                                     ): Option[Card] =
          Some(new TreasureCard("Foreign", 0, 1))
      }

      new Workshop().play(context(player, opponent, decisions))

      player.discardPileSize shouldBe 0
    }
  }

  "Woodcutter" should {

    "add two money and one buy" in {
      val player = new Player(1)
      val opponent = new Player(2)

      new Woodcutter().play(context(player, opponent))

      player.perRoundMoney shouldBe 2
      player.perRoundBuys shouldBe 2
    }
  }
}
