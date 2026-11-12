package dominion.model
import dominion.util.PlaySet

class Game(
            val players: Vector[Player],
            val gameMode: GameMode,
            val playSet: Option[PlaySet]
          ) {

  require(players.size >= 2 && players.size <= 8,
    "A game requires between 2 and 8 players.")

  require(players.map(_.id).distinct.size == players.size,
    "Player IDs must be unique.")

  private var currentPlayerIndex: Int = 0

  def currentPlayer: Player =
    players(currentPlayerIndex)

  def nextTurn(): Unit = {
    currentPlayerIndex =
      (currentPlayerIndex + 1) % players.size
  }
}