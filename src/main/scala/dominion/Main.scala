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

      val gameView = new GameView(message => println(message))
      gameView.attach(game)

      val gameController = new GameController(
        game = game,
        readInput = () => readLine(),
        printMessage = message => println(message)
      )
      gameController.run()
      gameView.detach()
    }
  )

  val view = new MainMenuView(message => println(message))
  controller.addObserver(view)
  controller.run()

  println("Game Terminated")
}
// $COVERAGE-ON$
