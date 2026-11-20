package dominion.model

import scala.util.Random

class Player(
              val id: Int,
              var perRoundActions: Int = 1,
              var perRoundMoney: Int = 0,
              var perRoundBuys: Int = 1
            ) {

  private val deck = new Deck
  private val discardPile = new DiscardPile
  private var hand: List[Card] = Nil

  def drawCard(): Option[Card] = {
    if (deck.isEmpty && !discardPile.isEmpty) {
      val shuffledCards = Random.shuffle(discardPile.removeAll())
      deck.addAllToTop(shuffledCards)
    }

    deck.draw()
  }

  /**
   * Draws up to `count` cards and adds them to the player's hand.
   * Returns the cards actually drawn (fewer may be available).
   */
  def drawCardsToHand(count: Int): List[Card] = {
    require(count >= 0, "Draw count cannot be negative")

    val drawnCards = List.fill(count)(drawCard()).flatten
    hand = hand ++ drawnCards
    drawnCards
  }

  def handCards: List[Card] =
    hand

  def handSize: Int =
    hand.size

  def discardCard(card: Card): Unit =
    discardPile.add(card)

  def discardPileCards(count: Int): List[Card] =
    discardPile.peekTop(count)

  def takeDiscardCard(index: Int): Option[Card] =
    discardPile.takeAt(index)

  def deckSize: Int =
    deck.size

  def discardPileSize: Int =
    discardPile.size

  def addCardToDeck(card: Card): Unit =
    deck.addToTop(card)

  def addCardsToDeck(cards: List[Card]): Unit =
    deck.addAllToTop(cards)

  def addActions(amount: Int): Unit = {
    require(amount >= 0, "Action amount cannot be negative")
    perRoundActions += amount
  }

  def addMoney(amount: Int): Unit = {
    require(amount >= 0, "Money amount cannot be negative")
    perRoundMoney += amount
  }

  def addBuys(amount: Int): Unit = {
    require(amount >= 0, "Buy amount cannot be negative")
    perRoundBuys += amount
  }
}
