package dominion.view

import dominion.model.*
import dominion.model.table.CardPile
import dominion.util.Observer

/** Text UI projection of the current supply. */
final class GameView(printMessage: String => Unit) extends Observer[GameEvent] {
  private var observedGame: Option[Game] = None

  def attach(game: Game): Unit = {
    observedGame.foreach(_.removeObserver(this))
    observedGame = Some(game)
    game.addObserver(this)
    renderSupply()
  }

  override def update(event: GameEvent): Unit = event match {
    case _: SupplyChanged | GameStateRestored => renderSupply()
    case _ => ()
  }

  def renderSupply(): Unit =
    observedGame.foreach(game => printMessage(GameView.renderSupply(game)))
}

object GameView {
  def renderSupply(game: Game): String = {
    val piles = game.playTable.allPiles.filterNot(_.isEmpty)
    val actions = piles.filter(_.card.isInstanceOf[ActionCard])
    val treasures = piles.filter(_.card.isInstanceOf[TreasureCard])
    val victories = piles.filter(_.card.isInstanceOf[VictoryCard])
    val others = piles.filterNot(p =>
      p.card.isInstanceOf[ActionCard] ||
      p.card.isInstanceOf[TreasureCard] ||
      p.card.isInstanceOf[VictoryCard]
    )

    val sections = List(
      renderSection("Aktionskarten", actions),
      renderSection("Schatzkarten", treasures),
      renderSection("Siegpunktkarten", victories),
      renderSection("Weitere Karten", others)
    ).filter(_.nonEmpty)

    val header = s"========== VORRAT (${piles.size} verfügbare Stapel) =========="
    val body = if (sections.isEmpty) "Keine Karten mehr im Vorrat." else sections.mkString("\n")
    s"$header\n$body\n${"=" * header.length}"
  }

  private def renderSection(title: String, piles: List[CardPile]): String =
    if (piles.isEmpty) ""
    else {
      val rows = piles.sortBy(p => (p.card.cost, p.card.name)).map { pile =>
        f"${pile.card.name}%-14s Kosten: ${pile.card.cost}%2d   Vorrat: ${pile.count}%2d"
      }
      (title +: rows).mkString("\n")
    }
}
