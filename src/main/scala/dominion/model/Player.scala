
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

  // Draw a card from the deck.
  // If the deck is empty, refill it from the discard pile.
  def drawCard(): Option[Card] = {

    if (deck.isEmpty && !discardPile.isEmpty) {

      val shuffledCards =
        Random.shuffle(discardPile.removeAll())

      deck.addAllToTop(shuffledCards)
    }

    deck.draw()
  }

  // Add a card to the discard pile.
  def discardCard(card: Card): Unit =
    discardPile.add(card)

  // Inspect the top cards of the discard pile.
  def discardPileCards(count: Int): List[Card] =
    discardPile.peekTop(count)

  // Remove a specific card by its position.
  def takeDiscardCard(index: Int): Option[Card] =
    discardPile.takeAt(index)

  // Information about the stacks.
  def deckSize: Int =
    deck.size

  def discardPileSize: Int =
    discardPile.size
}
