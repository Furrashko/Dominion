
package dominion.model

import dominion.util.Observable
import dominion.util.PlaySet

sealed trait GameEvent

final case class CurrentPlayerChanged(
                                       player: Player
                                     ) extends GameEvent

final case class TurnPhaseChanged(
                                   player: Player,
                                   phase: TurnPhase
                                 ) extends GameEvent

class Game(
            val players: Vector[Player],
            val gameMode: GameMode,
            val playSet: Option[PlaySet]
          ) extends Observable[GameEvent] {

  require(
    players.size >= 2 && players.size <= 8,
    "A game requires between 2 and 8 players."
  )

  require(
    players.map(_.id).distinct.size == players.size,
    "Player IDs must be unique."
  )

  private var currentPlayerIndex: Int = 0

  private var phase: TurnPhase =
    TurnPhase.Action

  def currentPlayer: Player =
    players(currentPlayerIndex)

  def currentPhase: TurnPhase =
    phase

  /**
   * Advances the game to the next phase.
   *
   * Action -> Buy -> Cleanup -> Next Player
   */
  def advancePhase(): Unit = {

    phase.next match {

      case Some(nextPhase) =>
        phase = nextPhase

        notifyObservers(
          TurnPhaseChanged(currentPlayer, phase)
        )

      case None =>
        nextTurn()
    }
  }

  /**
   * Advances to the next player.
   *
   * The next player starts in Action phase.
   * Only the player change is published.
   */
  def nextTurn(): Unit = {

    currentPlayerIndex =
      (currentPlayerIndex + 1) % players.size

    phase = TurnPhase.Action

    notifyObservers(
      CurrentPlayerChanged(currentPlayer)
    )
  }
}
