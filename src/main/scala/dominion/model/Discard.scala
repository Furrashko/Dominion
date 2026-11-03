
package dominion.model

class DiscardPile(initialCards: List[Card] = Nil) {

  private var cards: List[Card] = initialCards

  def isEmpty: Boolean =
    cards.isEmpty

  def size: Int =
    cards.size

  def peek: Option[Card] =
    cards.headOption

  def peekTop(count: Int): List[Card] = {
    require(count >= 0, "Count cannot be negative")
    cards.take(count)
  }

  def add(card: Card): Unit =
    cards = card :: cards

  def addAll(newCards: List[Card]): Unit =
    cards = newCards ::: cards

  def draw(): Option[Card] = {
    cards match {
      case head :: tail =>
        cards = tail
        Some(head)

      case Nil =>
        None
    }
  }

  def takeTop(count: Int): List[Card] = {
    require(count >= 0, "Count cannot be negative")

    val takenCards = cards.take(count)
    cards = cards.drop(count)

    takenCards
  }

  def takeAt(index: Int): Option[Card] = {
    if (index < 0 || index >= cards.size)
      None
    else {
      val (before, after) = cards.splitAt(index)
      val selectedCard = after.head
      cards = before ::: after.tail

      Some(selectedCard)
    }
  }

  def removeAll(): List[Card] = {
    val removedCards = cards
    cards = Nil
    removedCards
  }
}
