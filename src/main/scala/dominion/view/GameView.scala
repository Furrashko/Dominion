package dominion.view

import dominion.model.*
import dominion.model.table.CardPile
import dominion.util.Observer

/** Observer projection of the game state. */
final class GameView(printMessage: String => Unit) extends Observer[GameEvent] {
  private var observedGame: Option[Game] = None

  def attach(game: Game): Unit = {
    observedGame.foreach(_.removeObserver(this))
    observedGame = Some(game)
    game.addObserver(this)
    render()
  }

  def detach(): Unit = {
    observedGame.foreach(_.removeObserver(this))
    observedGame = None
  }

  override def update(event: GameEvent): Unit = event match {
    case _: GameUpdated | GameStateRestored => render()
    case _ => ()
  }

  def render(): Unit =
    observedGame.foreach(game => printMessage(GameView.render(game)))

  def renderSupply(): Unit =
    observedGame.foreach(game => printMessage(GameView.renderSupply(game)))
}

object GameView {
  def render(game: Game): String = {
    val player = game.currentPlayer
    val hand = player.handCards.zipWithIndex.map { case (card, index) =>
      f"${index + 1}%2d. ${card.name}%-14s Kosten: ${card.cost}%2d"
    }
    val played =
      if (player.playedCards.isEmpty) "—"
      else player.playedCards.map(_.name).mkString(", ")
    val scores = game.players
      .map(p => s"Spieler ${p.id}: ${p.victoryPoints} VP").mkString(" | ")

    val board =
      s"""========== DOMINION ==========
         |Aktiver Spieler: ${player.id}
         |Phase: ${game.currentPhase}
         |Aktionen: ${player.availableActions}   Käufe: ${player.availableBuys}   Geld: ${player.availableMoney}
         |Hand (${player.handSize}):
         |${if (hand.isEmpty) "  (leer)" else hand.map("  " + _).mkString("\n")}
         |Gespielt: $played
         |Nachziehstapel: ${player.deckSize}   Ablagestapel: ${player.discardPileSize}
         |Punkte: $scores
         |Undo: ${game.canUndo}   Redo: ${game.canRedo}
         |""".stripMargin

    val endStatus =
      if (game.isGameOver) {
        val highScore = game.currentScores.values.max
        val winners = game.currentScores.collect {
          case (id, score) if score == highScore => s"Spieler $id"
        }.mkString(", ")
        s"\nSPIEL BEENDET – Gewinner: $winners ($highScore VP)\n"
      } else ""

    board + endStatus + "\n" + renderSupply(game)
  }

  def renderSupply(game: Game): String = {
    val piles = game.playTable.allPiles.filterNot(_.isEmpty)
    val actions = piles.filter(_.card.isInstanceOf[ActionCard])
    val treasures = piles.filter(_.card.isInstanceOf[TreasureCard])
    val victories = piles.filter(_.card.isInstanceOf[VictoryCard])
    val others = piles.filterNot(pile =>
      pile.card.isInstanceOf[ActionCard] ||
        pile.card.isInstanceOf[TreasureCard] ||
        pile.card.isInstanceOf[VictoryCard]
    )

    val sections = List(
      renderSection("Aktionskarten", actions),
      renderSection("Schatzkarten", treasures),
      renderSection("Siegpunktkarten", victories),
      renderSection("Weitere Karten", others)
    ).filter(_.nonEmpty)

    val header = s"========== VORRAT (${piles.size} verfügbare Stapel) =========="
    val body = if (sections.isEmpty) "Keine Karten mehr im Vorrat."
      else sections.mkString("\n")
    s"$header\n$body\n${"=" * header.length}"
  }

  private def renderSection(title: String, piles: List[CardPile]): String =
    if (piles.isEmpty) ""
    else {
      val rows = piles.sortBy(pile => (pile.card.cost, pile.card.name)).map { pile =>
        f"${pile.card.name}%-14s Kosten: ${pile.card.cost}%2d   Vorrat: ${pile.count}%2d"
      }
      (title +: rows).mkString("\n")
    }
}
