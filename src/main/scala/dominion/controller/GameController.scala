package dominion.controller

import dominion.model.*
import dominion.util.{Observable, Observer}
import scala.util.control.NonFatal

/** Events consumed by GameView. The controller never performs presentation. */
sealed trait GameControllerEvent
object GameControllerEvent {
  case object ShowGame extends GameControllerEvent
  case object ShowHelp extends GameControllerEvent
  final case class ShowPrompt(message: String) extends GameControllerEvent
  final case class ShowMessage(message: String) extends GameControllerEvent
  final case class GameChanged(event: GameEvent) extends GameControllerEvent
}

/**
 * Handles commands and delegates all presentation to observers.
 * Game events are forwarded so the view only needs to observe this controller.
 */
final class GameController(
  val game: Game,
  readInput: () => String,
  decisions: DecisionProvider = DecisionProvider.default
) extends Observable[GameControllerEvent] {

  private val gameObserver: Observer[GameEvent] = new Observer[GameEvent] {
    override def update(event: GameEvent): Unit =
      notifyObservers(GameControllerEvent.GameChanged(event))
  }

  game.addObserver(gameObserver)

  private def handCard(index: Int): Card = {
    require(index >= 1 && index <= game.currentPlayer.handCards.size,
      s"Hand index must be between 1 and ${game.currentPlayer.handCards.size}")
    game.currentPlayer.handCards(index - 1)
  }

  def playHandCard(index: Int): Unit =
    (game.currentPhase, handCard(index)) match {
      case (TurnPhase.Action, card: ActionCard) => game.playAction(card, decisions)
      case (TurnPhase.Buy, card: TreasureCard) => game.playTreasure(card)
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
          if (!undo()) notifyObservers(GameControllerEvent.ShowMessage(
            "Nichts zum Rückgängigmachen vorhanden."))
        case "redo" =>
          if (!redo()) notifyObservers(GameControllerEvent.ShowMessage(
            "Nichts zum Wiederholen vorhanden."))
        case "status" | "show" =>
          notifyObservers(GameControllerEvent.ShowGame)
        case "help" => notifyObservers(GameControllerEvent.ShowHelp)
        case "quit" | "exit" => return false
        case other =>
          notifyObservers(GameControllerEvent.ShowMessage(
            s"Unbekannter Befehl: $other. 'help' zeigt die Befehle."))
      }
    } catch {
      case NonFatal(exception) =>
        notifyObservers(GameControllerEvent.ShowMessage(
          s"Zug abgelehnt: ${Option(exception.getMessage).getOrElse("Ungültiger Zug")}"))
    }
    true
  }

  def run(): Unit = {
    notifyObservers(GameControllerEvent.ShowHelp)
    notifyObservers(GameControllerEvent.ShowGame)

    var running = true
    while (running) {
      notifyObservers(GameControllerEvent.ShowPrompt(
        s"${game.currentPlayer.id}/${game.currentPhase} >"))
      val line = readInput()
      if (line == null) running = false
      else running = execute(line)
    }
  }

  def close(): Unit = game.removeObserver(gameObserver)

  private def parseHandIndex(value: String): Int =
    value.toIntOption.getOrElse {
      throw new IllegalArgumentException("Bitte einen gültigen Handkarten-Index angeben.")
    }
}
