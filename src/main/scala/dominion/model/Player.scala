
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

  def drawCard(): Option[Card] = {

    if (deck.isEmpty && !discardPile.isEmpty) {

      val shuffledCards =
        Random.shuffle(discardPile.removeAll())

      deck.addAllToTop(shuffledCards)
    }

    deck.draw()
  }

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
}
