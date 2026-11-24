package dominion.model

import dominion.model.table.PlayTable

/**
 * UI-independent decisions required by action cards.
 *
 * A TUI or GUI can implement this interface to ask the active user. The
 * default implementation makes deterministic legal choices and is useful for
 * tests and headless games.
 */
trait DecisionProvider {

  def chooseCardsToDiscard(
    player: Player,
    candidates: List[Card],
    count: Int
  ): List[Card] =
    candidates.take(count)

  /** Cellar permits discarding any number of cards, including zero. */
  def chooseCellarDiscards(
    player: Player,
    candidates: List[Card]
  ): List[Card] =
    Nil

  def chooseCardToTrash(
    player: Player,
    candidates: List[Card]
  ): Option[Card] =
    candidates.headOption

  def chooseCardToGain(
    player: Player,
    candidates: List[Card],
    maxCost: Int
  ): Option[Card] =
    candidates
      .filter(_.cost <= maxCost)
      .sortBy(card => (-card.cost, card.name))
      .headOption

  def shouldRevealMoat(defendingPlayer: Player): Boolean =
    defendingPlayer.handCards.exists(_.name == "Moat")
}

object DecisionProvider {
  val default: DecisionProvider = new DecisionProvider {}
}

/**
 * Runtime context passed to action cards.
 *
 * It deliberately contains model-level abstractions only; it has no dependency
 * on a terminal, GUI toolkit or input framework.
 */
final class ActionContext(
  val game: Game,
  val player: Player,
  val table: PlayTable,
  val decisions: DecisionProvider
) {

  def discardFromHand(target: Player, selected: List[Card]): Unit = {
    require(
      selected.distinct.size == selected.size,
      "A card cannot be selected more than once"
    )
    require(
      selected.forall(card => target.handCards.exists(_ eq card)),
      "Every selected card must be in the player's hand"
    )
    selected.foreach { card =>
      require(target.removeCardFromHand(card), "Selected card is no longer in hand")
      target.discardCard(card)
    }
  }

  def trashFromHand(target: Player, card: Card): Unit = {
    require(target.removeCardFromHand(card), "Card is no longer in hand")
  }

  /** Gain the matching supply card to the player's discard pile. */
  def gainToDiscard(card: Card): Option[Card] =
    table.buyCard(card.name).map { gained =>
      player.discardCard(gained)
      gained
    }

  /** Gain the matching supply card directly to the player's hand. */
  def gainToHand(card: Card): Option[Card] =
    table.buyCard(card.name).map { gained =>
      player.addCardToHand(gained)
      gained
    }
}
