package dominion.model

import dominion.model.table.PlayTable
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

  require(players.size >= 2 && players.size <= 8, "A game requires between 2 and 8 players.")
  require(players.map(_.id).distinct.size == players.size, "Player IDs must be unique.")

  private var currentPlayerIndex = 0
  private var phase = TurnPhase.Action
  private var ended = false
  private var past = List.empty[GameSnapshot]
  private var future = List.empty[GameSnapshot]

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
    players.zip(state.players).foreach { case (player, playerState) => player.restore(playerState) }
    playTable.restore(state.table)
  }

  private def execute[A](operation: => A): A = {
    ensureGameRunning()
    val before = snapshot
    try {
      val result = operation
      past = before :: past
      future = Nil
      result
    } catch {
      case exception: Throwable =>
        restore(before)
        throw exception
    }
  }

  def playAction(card: ActionCard, decisions: DecisionProvider = DecisionProvider.default): Unit = execute {
    ensurePhase(TurnPhase.Action)
    require(currentPlayer.availableActions > 0, "No actions available")
    require(currentPlayer.handCards.exists(_ eq card), "Card is not in the player's hand")
    currentPlayer.useAction()
    require(currentPlayer.playCard(card), "Card could not be played")
    card.play(new ActionContext(this, currentPlayer, playTable, decisions))
    notifyObservers(CardPlayed(currentPlayer, card))
    notifyObservers(ResourcesChanged(currentPlayer))
  }

  def playTreasure(card: TreasureCard): Unit = execute {
    ensurePhase(TurnPhase.Buy)
    require(currentPlayer.handCards.exists(_ eq card), "Card is not in the player's hand")
    require(currentPlayer.playTreasure(card), "Treasure could not be played")
    notifyObservers(CardPlayed(currentPlayer, card))
    notifyObservers(ResourcesChanged(currentPlayer))
  }

  def buyCard(name: String): Unit = execute {
    ensurePhase(TurnPhase.Buy)
    val pile = playTable.getPile(name).getOrElse {
      throw new IllegalArgumentException(s"Unknown card pile: $name")
    }
    require(!pile.isEmpty, s"$name pile is empty")
    val card = pile.card
    require(currentPlayer.availableBuys > 0, "No buys available")
    require(currentPlayer.availableMoney >= card.cost, "Not enough money")
    currentPlayer.spendMoney(card.cost)
    currentPlayer.useBuy()
    val boughtCard = playTable.buyCard(name).get
    currentPlayer.discardCard(boughtCard)
    notifyObservers(CardBought(currentPlayer, boughtCard))
    notifyObservers(SupplyChanged(boughtCard, pile.count))
    notifyObservers(ResourcesChanged(currentPlayer))
    checkGameEnd()
  }

  /** Called by ActionContext while playAction owns the transaction/history snapshot. */
  private[model] def gainFromSupply(name: String): Option[Card] = {
    val pile = playTable.getPile(name)
    val gained = playTable.buyCard(name)
    gained.foreach(card => notifyObservers(SupplyChanged(card, pile.map(_.count).getOrElse(0))))
    gained
  }

  def advancePhase(): Unit = execute {
    phase match {
      case TurnPhase.Action =>
        phase = TurnPhase.Buy
        notifyObservers(TurnPhaseChanged(currentPlayer, phase))
      case TurnPhase.Buy =>
        phase = TurnPhase.Cleanup
        notifyObservers(TurnPhaseChanged(currentPlayer, phase))
      case TurnPhase.Cleanup =>
        cleanupCurrentPlayer()
        currentPlayerIndex = (currentPlayerIndex + 1) % players.size
        phase = TurnPhase.Action
        currentPlayer.resetTurnResources()
        val drawn = currentPlayer.drawCardsToHand(5)
        notifyObservers(CardDrawn(currentPlayer, drawn))
        notifyObservers(CurrentPlayerChanged(currentPlayer))
        notifyObservers(TurnPhaseChanged(currentPlayer, phase))
        notifyObservers(ResourcesChanged(currentPlayer))
    }
  }

  private def cleanupCurrentPlayer(): Unit = {
    val player = currentPlayer
    player.discardHand()
    player.discardPlayedCards()
    player.resetTurnResources()
    val drawn = player.drawCardsToHand(5)
    notifyObservers(CardDrawn(player, drawn))
  }

  def nextTurn(): Unit = execute {
    currentPlayerIndex = (currentPlayerIndex + 1) % players.size
    phase = TurnPhase.Action
    currentPlayer.resetTurnResources()
    notifyObservers(CurrentPlayerChanged(currentPlayer))
  }

  private def checkGameEnd(): Unit = {
    if (playTable.isGameOver) {
      ended = true
      val scores = currentScores
      val highestScore = scores.values.max
      val winners = scores.collect { case (id, score) if score == highestScore => id }.toList
      notifyObservers(GameEnded(scores, winners))
    }
  }

  def undo(): Boolean = past match {
    case previous :: remaining =>
      future = snapshot :: future
      past = remaining
      restore(previous)
      notifyObservers(GameStateRestored)
      true
    case Nil => false
  }

  def redo(): Boolean = future match {
    case next :: remaining =>
      past = snapshot :: past
      future = remaining
      restore(next)
      notifyObservers(GameStateRestored)
      true
    case Nil => false
  }
}
