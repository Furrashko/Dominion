package dominion

import scala.io.StdIn.readLine

import dominion.controller.MainMenuController
import dominion.model.Game

// $COVERAGE-OFF$
@main def main(): Unit = {

  println("Dominion")

  val controller = new MainMenuController(
    readInput = () => readLine(),
    printMessage = message => println(message),
    startGame = (game: Game) => {
      println("Game successfully initialized.")
      println(s"Players: ${game.players.size}")
      println(s"Game Mode: ${game.gameMode}")
      println(s"PlaySet: ${game.playSet}")
    }
  )

  controller.run()

  println("Game Terminated")
}
// $COVERAGE-ON$