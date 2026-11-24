package dominion.view

import dominion.controller.{GameController, GameControllerEvent}
import dominion.model.*
import dominion.model.table.CardPile
import dominion.util.Observer
import scala.annotation.tailrec

/** TUI view: observes controller events and handles interactive card decisions. */
final class GameView(
  readInput: () => String,
  printMessage: String => Unit
) extends Observer[GameControllerEvent] with DecisionProvider {

  private var observedController: Option[GameController] = None
  private var observedGame: Option[Game] = None

  def attach(controller: GameController): Unit = {
    detach()
    observedController = Some(controller)
    observedGame = Some(controller.game)
    controller.addObserver(this)
  }

  def detach(): Unit = {
    observedController.foreach(_.removeObserver(this))
    observedController = None
    observedGame = None
  }

  override def update(event: GameControllerEvent): Unit = event match {
    case GameControllerEvent.ShowGame => render()
    case GameControllerEvent.ShowHelp => showHelp()
    case GameControllerEvent.ShowPrompt(message) => printMessage(message)
    case GameControllerEvent.ShowMessage(message) => printMessage(message)
    case GameControllerEvent.GameChanged(_: GameUpdated) => render()
    case GameControllerEvent.GameChanged(GameStateRestored) => ()
    case GameControllerEvent.GameChanged(_) => ()
  }

  def render(): Unit =
    observedGame.foreach(game => printMessage(GameView.render(game)))

  def renderSupply(): Unit =
    observedGame.foreach(game => printMessage(GameView.renderSupply(game)))

  private def showHelp(): Unit =
    printMessage(
      """Befehle:
        |  play <index>       Aktionskarte (Action) oder Schatzkarte (Buy) spielen
        |  treasure <index>   Schatzkarte spielen (Buy)
        |  buy <Kartenname>   Karte aus dem Vorrat kaufen
        |  phase              Zur nächsten Phase wechseln
        |  undo               Letzten Spielzug rückgängig machen
        |  redo               Rückgängig gemachten Spielzug wiederholen
        |  status             Spielzustand anzeigen
        |  help               Diese Hilfe anzeigen
        |  quit               Spiel verlassen
        |
        |Handkarten sind mit 1-basierten Indizes nummeriert.""".stripMargin
    )

  override def chooseCardsToDiscard(
    player: Player,
    candidates: List[Card],
    count: Int
  ): List[Card] = {
    if (count <= 0) Nil
    else {
      printCards(s"Spieler ${player.id}: Wähle genau $count Karte(n) zum Abwerfen.", candidates)
      readIndices(candidates, Some(count), allowEmpty = false)
        .getOrElse(candidates.indices.take(count).toList)
        .map(candidates)
    }
  }

  override def chooseCellarDiscards(
    player: Player,
    candidates: List[Card]
  ): List[Card] = {
    printCards(s"Spieler ${player.id}: Cellar – Karten abwerfen? Leer = keine.", candidates)
    readIndices(candidates, None, allowEmpty = true).getOrElse(Nil).map(candidates)
  }

  override def chooseCardToTrash(
    player: Player,
    candidates: List[Card]
  ): Option[Card] = {
    if (candidates.isEmpty) None
    else {
      printCards(s"Spieler ${player.id}: Karte zerstören? Leer = keine.", candidates)
      readIndices(candidates, Some(1), allowEmpty = true)
        .flatMap(_.headOption).map(candidates)
    }
  }

  override def chooseCardToGain(
    player: Player,
    candidates: List[Card],
    maxCost: Int
  ): Option[Card] = {
    if (candidates.isEmpty) None
    else {
      printCards(s"Spieler ${player.id}: Karte bis Kosten $maxCost gewinnen? Leer = keine.", candidates)
      readIndices(candidates, Some(1), allowEmpty = true)
        .flatMap(_.headOption).map(candidates)
    }
  }

  override def shouldRevealMoat(defendingPlayer: Player): Boolean = {
    printMessage(s"Spieler ${defendingPlayer.id}: Moat aufdecken? (j/n)")
    Option(readInput()).getOrElse("").trim.toLowerCase match {
      case "j" | "ja" | "y" | "yes" => true
      case _ => false
    }
  }

  private def printCards(prompt: String, cards: List[Card]): Unit = {
    printMessage(prompt)
    cards.zipWithIndex.foreach { case (card, index) =>
      printMessage(s"  ${index + 1}. ${card.name} (Kosten ${card.cost})")
    }
  }

  private def readIndices(
    cards: List[Card],
    requiredCount: Option[Int],
    allowEmpty: Boolean
  ): Option[List[Int]] = {
    @tailrec
    def loop(): Option[List[Int]] = {
      val raw = readInput()
      if (raw == null) None
      else {
        val value = raw.trim
        if (value.isEmpty && allowEmpty) Some(Nil)
        else {
          val indices = value.split("[,\\s]+").toList
            .flatMap(_.toIntOption).map(_ - 1)
          val valid =
            indices.nonEmpty &&
              indices.distinct.size == indices.size &&
              indices.forall(index => index >= 0 && index < cards.size) &&
              requiredCount.forall(_ == indices.size)

          if (valid) Some(indices)
          else {
            val countHint = requiredCount.fold("beliebig viele")(_.toString)
            printMessage(
              s"Ungültige Auswahl. Gib $countHint unterschiedliche Indizes aus 1 bis ${cards.size} ein."
            )
            loop()
          }
        }
      }
    }
    loop()
  }
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
