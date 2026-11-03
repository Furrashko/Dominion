
package dominion.model

import scala.util.Random

class Deck(initialCards: List[Card] = Nil) {

  private var cards: List[Card] = initialCards

  def isEmpty: Boolean =
    cards.isEmpty

  def size: Int =
    cards.size

  def peek: Option[Card] =
    cards.headOption

  def draw(): Option[Card] = {
    cards match {
      case head :: tail =>
        cards = tail
        Some(head)

      case Nil =>
        None
    }
  }

  def addToTop(card: Card): Unit =
    cards = card :: cards

  def addToBottom(card: Card): Unit =
    cards = cards :+ card

  def addAllToTop(newCards: List[Card]): Unit =
    cards = newCards ::: cards

  def shuffle(): Unit =
    cards = Random.shuffle(cards)

  def clear(): Unit =
    cards = Nil
}
