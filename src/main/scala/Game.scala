class Game(playerCount: Int) {

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