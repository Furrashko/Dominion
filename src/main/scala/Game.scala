class Game(playerCount: Int) {

  require(
    playerCount >= 2 && playerCount <= 8,
    "A partie need 2 to 8 players."
  )

  val players: Vector[Player] =
    (1 to playerCount).map(id => new Player(id)).toVector

  private var currentPlayerIndex: Int = 0

  def currentPlayer: Player =
    players(currentPlayerIndex)

  def nextTurn(): Unit = {
    currentPlayerIndex =
      (currentPlayerIndex + 1) % players.size
  }
}