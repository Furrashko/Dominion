
package dominion.model

import scala.util.Random

private[model] case class PlayerSnapshot(
                                          deck: List[Card],
                                          discardPile: List[Card],
                                          hand: List[Card],
                                          playedCards: List[Card],
                                          actions: Int,
                                          money: Int,
                                          buys: Int,
                                          treasureMoneySpent: Int
                                        )

class Player(
              val id: Int,
              var perRoundActions: Int = 1,
              var perRoundMoney: Int = 0,
              var perRoundBuys: Int = 1
            ) {

  private val deck = new Deck
  private val discardPile = new DiscardPile

  private var hand: List[Card] = Nil
  private var played: List[Card] = Nil
  private var treasureMoneySpent = 0

  def handCards: List[Card] = hand

  def playedCards: List[Card] = played

  def handSize: Int = hand.size

  def deckSize: Int = deck.size

  def discardPileSize: Int = discardPile.size

  def availableActions: Int = perRoundActions

  def availableMoney: Int =
    perRoundMoney +
      played.collect {
        case card: TreasureCard => card.value
      }.sum -
      treasureMoneySpent

  def availableBuys: Int = perRoundBuys

  def victoryPoints: Int =
    (
      hand ++
        played ++
        discardPileCards(discardPileSize) ++
        deck.cardsSnapshot
      ).collect {
      case card: VictoryCard => card.victoryPoints
    }.sum

  def drawCard(): Option[Card] = {
    if (deck.isEmpty && !discardPile.isEmpty) {
      val shuffledCards =
        Random.shuffle(discardPile.removeAll())

      deck.addAllToTop(shuffledCards)
    }

    deck.draw()
  }

  def drawCardsToHand(count: Int): List[Card] = {
    require(count >= 0, "Draw count cannot be negative")

    val drawnCards =
      List.fill(count)(drawCard()).flatten

    hand = hand ++ drawnCards
    drawnCards
  }

  def addCardToHand(card: Card): Unit =
    hand = hand :+ card

  def removeCardFromHand(card: Card): Boolean = {
    val index = hand.indexWhere(_ eq card)

    if (index < 0) false
    else {
      hand = hand.patch(index, Nil, 1)
      true
    }
  }

  def playCard(card: Card): Boolean = {
    if (removeCardFromHand(card)) {
      played = played :+ card
      true
    } else false
  }

  def playTreasure(card: TreasureCard): Boolean =
    playCard(card)

  def discardCard(card: Card): Unit =
    discardPile.add(card)

  def discardHand(): Unit = {
    discardPile.addAll(hand)
    hand = Nil
  }

  def discardPlayedCards(): Unit = {
    discardPile.addAll(played)
    played = Nil
  }

  def discardPileCards(count: Int): List[Card] =
    discardPile.peekTop(count)

  def takeDiscardCard(index: Int): Option[Card] =
    discardPile.takeAt(index)

  def addCardToDeck(card: Card): Unit =
    deck.addToTop(card)

  def addCardsToDeck(cards: List[Card]): Unit =
    deck.addAllToTop(cards)

  def addActions(amount: Int): Unit = {
    require(amount >= 0, "Action amount cannot be negative")
    perRoundActions += amount
  }

  def useAction(): Unit = {
    require(perRoundActions > 0, "No actions available")
    perRoundActions -= 1
  }

  def addMoney(amount: Int): Unit = {
    require(amount >= 0, "Money amount cannot be negative")
    perRoundMoney += amount
  }

  def spendMoney(amount: Int): Unit = {
    require(amount >= 0, "Money amount cannot be negative")
    require(availableMoney >= amount, "Not enough money")

    val fromBonus = amount.min(perRoundMoney)

    perRoundMoney -= fromBonus
    treasureMoneySpent += amount - fromBonus
  }

  def addBuys(amount: Int): Unit = {
    require(amount >= 0, "Buy amount cannot be negative")
    perRoundBuys += amount
  }

  def useBuy(): Unit = {
    require(perRoundBuys > 0, "No buys available")
    perRoundBuys -= 1
  }

  def resetTurnResources(): Unit = {
    perRoundActions = 1
    perRoundMoney = 0
    perRoundBuys = 1
    treasureMoneySpent = 0
  }

  private[model] def snapshot: PlayerSnapshot =
    PlayerSnapshot(
      deck.cardsSnapshot,
      discardPile.cardsSnapshot,
      hand,
      played,
      perRoundActions,
      perRoundMoney,
      perRoundBuys,
      treasureMoneySpent
    )

  private[model] def restore(
                              state: PlayerSnapshot
                            ): Unit = {
    deck.restore(state.deck)
    discardPile.restore(state.discardPile)

    hand = state.hand
    played = state.playedCards

    perRoundActions = state.actions
    perRoundMoney = state.money
    perRoundBuys = state.buys

    treasureMoneySpent = state.treasureMoneySpent
  }
}
