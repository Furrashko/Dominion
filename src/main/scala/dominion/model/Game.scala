package dominion.model

import dominion.model.playset.PlaySetCatalog
import dominion.model.table.{PlayTable, PlayTableFactory}
import dominion.util.{Observable, PlaySet}

sealed trait GameEvent
final case class CurrentPlayerChanged(player: Player) extends GameEvent
final case class TurnPhaseChanged(player: Player, phase: TurnPhase) extends GameEvent
final case class CardPlayed(player: Player, card: Card) extends GameEvent
final case class CardBought(player: Player, card: Card) extends GameEvent
final case class CardDrawn(player: Player, cards: List[Card]) extends GameEvent
final case class ResourcesChanged(player: Player) extends GameEvent
final case class SupplyChanged(card: Card, remaining: Int) extends GameEvent
final case class GameEnded(scores: Map[Int, Int], winnerIds: List[Int]) extends GameEvent
final case class GameUpdated(reason: String) extends GameEvent
case object GameStateRestored extends GameEvent

private case class GameSnapshot(
  currentPlayerIndex: Int,
  phase: TurnPhase,
  players: Vector[PlayerSnapshot],
  table: Map[String, Int],
  ended: Boolean
)

class Game(
  val players: Vector[Player],
  val gameMode: GameMode,
  val playSet: Option[PlaySet],
  val playTable: PlayTable
) extends Observable[GameEvent] {

  /** Backwards-compatible constructor for callers that do not pass a table. */
  def this(
    players: Vector[Player],
    gameMode: GameMode,
    playSet: Option[PlaySet]
  ) = this(
    players,
    gameMode,
    playSet,
    PlayTableFactory.create(
      players.size,
      PlaySetCatalog.definition(playSet.getOrElse(PlaySet.FirstGame))
    )
  )

  require(players.size >= 2 && players.size <= 8,
    "A game requires between 2 and 8 players.")
  require(players.map(_.id).distinct.size == players.size,
    "Player IDs must be unique.")

  private var currentPlayerIndex = 0
  private var phase = TurnPhase.Action
  private var ended = false
  private var past = List.empty[GameSnapshot]
  private var future = List.empty[GameSnapshot]
  private var bufferedEvents: Option[Vector[GameEvent]] = None

  def currentPlayer: Player = players(currentPlayerIndex)
  def currentPhase: TurnPhase = phase
  def isGameOver: Boolean = ended
  def canUndo: Boolean = past.nonEmpty
  def canRedo: Boolean = future.nonEmpty

  def currentScores: Map[Int, Int] =
    players.map(player => player.id -> player.victoryPoints).toMap

  private def ensureGameRunning(): Unit =
    require(!ended, "The game has already ended")

  private def ensurePhase(expected: TurnPhase): Unit = {
    ensureGameRunning()
    require(phase == expected, s"Action requires phase $expected")
  }

  private def snapshot: GameSnapshot =
    GameSnapshot(currentPlayerIndex, phase, players.map(_.snapshot), playTable.snapshot, ended)

  private def restore(state: GameSnapshot): Unit = {
    currentPlayerIndex = state.currentPlayerIndex
    phase = state.phase
    ended = state.ended
    players.zip(state.players).foreach { case (player, playerState) =>
      player.restore(playerState)
    }
    playTable.restore(state.table)
  }

  private def emit(event: GameEvent): Unit =
    bufferedEvents match {
      case Some(events) => bufferedEvents = Some(events :+ event)
      case None => notifyObservers(event)
    }

  /** Failed commands restore the exact previous state and publish no events. */
  private def execute[A](reason: String)(operation: => A): A = {
    ensureGameRunning()
    val before = snapshot
    bufferedEvents = Some(Vector.empty)

    val result =
      try {
        val value = operation
        checkGameEnd()
        value
      } catch {
        case exception: Throwable =>
          restore(before)
          bufferedEvents = None
          throw exception
      }

    val committedEvents = bufferedEvents.getOrElse(Vector.empty) :+ GameUpdated(reason)
    past = before :: past
    future = Nil
    bufferedEvents = None
    committedEvents.foreach(notifyObservers)
    result
  }

  def playAction(
    card: ActionCard,
    decisions: DecisionProvider = DecisionProvider.default
  ): Unit = execute(s"play:${card.name}") {
    ensurePhase(TurnPhase.Action)
    require(currentPlayer.availableActions > 0, "No actions available")
    require(currentPlayer.handCards.exists(_ eq card),
      "Card is not in the player's hand")

    currentPlayer.useAction()
    require(currentPlayer.playCard(card), "Card could not be played")
    card.play(new ActionContext(this, currentPlayer, playTable, decisions))
    emit(CardPlayed(currentPlayer, card))
    emit(ResourcesChanged(currentPlayer))
  }

  def playTreasure(card: TreasureCard): Unit =
    execute(s"treasure:${card.name}") {
      ensurePhase(TurnPhase.Buy)
      require(currentPlayer.handCards.exists(_ eq card),
        "Card is not in the player's hand")
      require(currentPlayer.playTreasure(card), "Treasure could not be played")
      emit(CardPlayed(currentPlayer, card))
      emit(ResourcesChanged(currentPlayer))
    }

  def buyCard(name: String): Unit = execute(s"buy:$name") {
    ensurePhase(TurnPhase.Buy)
    val pile = findPile(name).getOrElse {
      throw new IllegalArgumentException(s"Unknown card pile: $name")
    }
    require(!pile.isEmpty, s"${pile.card.name} pile is empty")
    val card = pile.card
    require(currentPlayer.availableBuys > 0, "No buys available")
    require(currentPlayer.availableMoney >= card.cost, "Not enough money")

    currentPlayer.spendMoney(card.cost)
    currentPlayer.useBuy()
    val boughtCard = playTable.buyCard(card.name).getOrElse {
      throw new IllegalStateException(s"${card.name} pile became empty")
    }
    currentPlayer.discardCard(boughtCard)
    emit(CardBought(currentPlayer, boughtCard))
    emit(SupplyChanged(boughtCard, pile.count))
    emit(ResourcesChanged(currentPlayer))
  }

  private def findPile(name: String) =
    playTable.allPiles.find(_.card.name.equalsIgnoreCase(name.trim))

  /** Called by ActionContext inside the surrounding playAction transaction. */
  private[model] def gainFromSupply(name: String): Option[Card] = {
    val gained = findPile(name).flatMap(pile => playTable.buyCard(pile.card.name))
    gained.foreach { card =>
      emit(SupplyChanged(card, findPile(card.name).map(_.count).getOrElse(0)))
    }
    gained
  }

  /** Action -> Buy -> Cleanup -> next player's Action. */
  def advancePhase(): Unit = execute("advance-phase") {
    phase match {
      case TurnPhase.Action =>
        phase = TurnPhase.Buy
        emit(TurnPhaseChanged(currentPlayer, phase))
      case TurnPhase.Buy =>
        phase = TurnPhase.Cleanup
        emit(TurnPhaseChanged(currentPlayer, phase))
      case TurnPhase.Cleanup =>
        currentPlayer.discardHand()
        currentPlayer.discardPlayedCards()
        currentPlayerIndex = (currentPlayerIndex + 1) % players.size
        phase = TurnPhase.Action
        currentPlayer.resetTurnResources()
        val drawn = currentPlayer.drawCardsToHand(5)
        emit(CardDrawn(currentPlayer, drawn))
        emit(CurrentPlayerChanged(currentPlayer))
        emit(TurnPhaseChanged(currentPlayer, phase))
        emit(ResourcesChanged(currentPlayer))
    }
  }

  /**
   * Compatibility helper that completes the rest of the current turn.
   * It still passes through the normal phases and performs Cleanup.
   */
  def nextTurn(): Unit =
    phase match {
      case TurnPhase.Action =>
        advancePhase()
        advancePhase()
        advancePhase()
      case TurnPhase.Buy =>
        advancePhase()
        advancePhase()
      case TurnPhase.Cleanup =>
        advancePhase()
    }

  private def checkGameEnd(): Unit = {
    if (!ended && playTable.isGameOver) {
      ended = true
      val scores = currentScores
      val highestScore = scores.values.max
      val winners = scores.collect {
        case (id, score) if score == highestScore => id
      }.toList
      emit(GameEnded(scores, winners))
    }
  }

  def undo(): Boolean =
    past match {
      case previous :: remaining =>
        future = snapshot :: future
        past = remaining
        restore(previous)
        notifyObservers(GameStateRestored)
        notifyObservers(GameUpdated("undo"))
        true
      case Nil => false
    }

  def redo(): Boolean =
    future match {
      case next :: remaining =>
        past = snapshot :: past
        future = remaining
        restore(next)
        notifyObservers(GameStateRestored)
        notifyObservers(GameUpdated("redo"))
        true
      case Nil => false
    }
}
