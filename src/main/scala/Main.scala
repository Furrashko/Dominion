@main def main(): Unit = {
  println("Dominion")

  val game = new Game(4)
  println(game.currentPlayer.id)
  game.nextTurn()
  println(game.currentPlayer.id)
}
