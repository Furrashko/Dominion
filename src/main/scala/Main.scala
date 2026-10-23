import scala.io.StdIn.readLine

@main def main(): Unit = {
  println("Dominion")
  println("Enter number of Players:")
  
  val players = readLine()
  
  require()
  
  val numberOfPLayers = players.toInt
  
  println(s"Number of Players: $numberOfPLayers")
  
  val game = new Game(numberOfPLayers)
  println(game.currentPlayer.id)
  game.nextTurn()
  println(game.currentPlayer.id)
  game.nextTurn()
  println(game.currentPlayer.id)
  game.nextTurn()
  println(game.currentPlayer.id)
  game.nextTurn()
  println(game.currentPlayer.id)
  game.nextTurn()
  println(game.currentPlayer.id)
}