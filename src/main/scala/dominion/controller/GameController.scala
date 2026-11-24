package dominion.controller

import dominion.model.*
import scala.annotation.tailrec
import scala.util.control.NonFatal

/**
 * Shared application API for TUI, GUI, bots and network clients.
 * Rules and state mutations remain exclusively inside Game.
 */
final class GameController(
  val game: Game,
  readInput: () => String,
  printMessage: String => Unit
) {
  private val decisions = new ConsoleDecisionProvider(readInput, printMessage)

  private def handCard(index: Int): Card = {
    require(index >= 1 && index <= game.currentPlayer.handCards.size,
      s"Hand index must be between 1 and ${game.currentPlayer.handCards.size}")
    game.currentPlayer.handCards(index - 1)
  }

  def playHandCard(index: Int): Unit =
    (game.currentPhase, handCard(index)) match {
      case (TurnPhase.Action, card: ActionCard) =>
        game.playAction(card, decisions)
      case (TurnPhase.Buy, card: TreasureCard) =>
        game.playTreasure(card)
      case (TurnPhase.Action, _: TreasureCard) =>
        throw new IllegalArgumentException("Treasure cards can only be played during Buy")
      case (TurnPhase.Buy, _: ActionCard) =>
        throw new IllegalArgumentException("Action cards can only be played during Action")
      case (TurnPhase.Cleanup, _) =>
        throw new IllegalArgumentException("No cards can be played during Cleanup")
      case (_, card) =>
        throw new IllegalArgumentException(s"${card.name} cannot be played directly")
    }

  def playAction(index: Int): Unit = {
    val card = handCard(index)
    require(game.currentPhase == TurnPhase.Action,
      "Action cards can only be played during Action")
    require(card.isInstanceOf[ActionCard], "Selected card is not an ActionCard")
    game.playAction(card.asInstanceOf[ActionCard], decisions)
  }

  def playTreasure(index: Int): Unit = {
    val card = handCard(index)
    require(game.currentPhase == TurnPhase.Buy,
      "Treasure cards can only be played during Buy")
    require(card.isInstanceOf[TreasureCard], "Selected card is not a TreasureCard")
    game.playTreasure(card.asInstanceOf[TreasureCard])
  }

  def buy(cardName: String): Unit = game.buyCard(cardName)
  def advancePhase(): Unit = game.advancePhase()
  def undo(): Boolean = game.undo()
  def redo(): Boolean = game.redo()

  /** Returns false only when the input loop should terminate. */
  def execute(command: String): Boolean = {
    val input = Option(command).getOrElse("").trim
    if (input.isEmpty) return true

    val separator = input.indexWhere(_.isWhitespace)
    val action = if (separator < 0) input.toLowerCase
      else input.substring(0, separator).toLowerCase
    val argument = if (separator < 0) ""
      else input.substring(separator).trim

    try {
      action match {
        case "play" => playHandCard(parseHandIndex(argument))
        case "treasure" => playTreasure(parseHandIndex(argument))
        case "buy" =>
          require(argument.nonEmpty, "Usage: buy <card name>")
          buy(argument)
        case "phase" | "end" => advancePhase()
        case "undo" =>
          if (!undo()) printMessage("Nichts zum Rückgängigmachen vorhanden.")
        case "redo" =>
          if (!redo()) printMessage("Nichts zum Wiederholen vorhanden.")
        case "status" | "show" =>
          printMessage(dominion.view.GameView.render(game))
        case "help" => printHelp()
        case "quit" | "exit" => return false
        case other =>
          printMessage(s"Unbekannter Befehl: $other. 'help' zeigt die Befehle.")
      }
    } catch {
      case NonFatal(exception) =>
        printMessage(s"Zug abgelehnt: ${Option(exception.getMessage).getOrElse("Ungültiger Zug")}")
    }
    true
  }

  def run(): Unit = {
    printHelp()
    printMessage(dominion.view.GameView.render(game))
    var running = true
    while (running) {
      printMessage(s"${game.currentPlayer.id}/${game.currentPhase} >")
      val line = readInput()
      if (line == null) running = false
      else running = execute(line)
    }
  }

  private def parseHandIndex(value: String): Int =
    value.toIntOption.getOrElse {
      throw new IllegalArgumentException("Bitte einen gültigen Handkarten-Index angeben.")
    }

  private def printHelp(): Unit =
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
}

/** Console implementation of the card-decision interface. */
final class ConsoleDecisionProvider(
  readInput: () => String,
  printMessage: String => Unit
) extends DecisionProvider {

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
