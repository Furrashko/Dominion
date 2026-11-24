package dominion

import scala.io.StdIn.readLine

import dominion.controller.{GameController, MainMenuController}
import dominion.model.Game
import dominion.view.{GameView, MainMenuView}

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
      println(s"PlaySet: ${game.playSet.map(_.displayName).getOrElse("Custom")}")

      val readGameInput = () => readLine()
      val gameView = new GameView(
        readInput = readGameInput,
        printMessage = message => println(message)
      )
      val gameController = new GameController(
        game = game,
        readInput = readGameInput,
        decisions = gameView
      )

      gameView.attach(gameController)
      try gameController.run()
      finally {
        gameView.detach()
        gameController.close()
      }
    }
  )

  val view = new MainMenuView(message => println(message))
  controller.addObserver(view)
  controller.run()

  println("Game Terminated")
}
// $COVERAGE-ON$
