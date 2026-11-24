
package dominion.model.table

import dominion.model.Card

final class CardPile(
                      val card: Card,
                      initialAmount: Int
                    ) {
  require(initialAmount >= 0, "Initial amount cannot be negative")

  private var amount: Int = initialAmount

  def count: Int = amount

  def isEmpty: Boolean = amount == 0

  def take(): Option[Card] = {
    if (isEmpty) None
    else {
      amount -= 1
      Some(card)
    }
  }

  private[model] def restore(value: Int): Unit = {
    require(value >= 0, "Pile amount cannot be negative")
    amount = value
  }
}
