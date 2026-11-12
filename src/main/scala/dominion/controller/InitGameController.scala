
package dominion.controller

import scala.annotation.tailrec

import dominion.model.*
import dominion.view.InitGameView
import dominion.util.PlaySet

object InitGameController {

  def parseNumberInput(
                        input: String,
                        min: Int,
                        max: Int
                      ): Option[Int] =
    input.toIntOption.filter(number =>
      number >= min && number <= max
    )
}

class InitGameController(
                          readInput: () => String,
                          printMessage: String => Unit
                        ) {

  private val view = new InitGameView(printMessage)

  private def readNumberInput(
                               min: Int,
                               max: Int
                             ): Option[Int] = {

    @tailrec
    def loop(): Option[Int] = {

      printMessage(s"Enter an integer between $min and $max:")

      Option(readInput()) match {

        case None =>
          None

        case Some(input) =>
          InitGameController.parseNumberInput(
            input,
            min,
            max
          ) match {

            case Some(number) =>
              Some(number)

            case None =>
              view.showInvalidInput(min, max)
              loop()
          }
      }
    }

    loop()
  }

  def createGame(
                  playerCount: Int,
                  gameMode: GameMode,
                  playSet: Option[PlaySet]
                ): Game = {

    val players =
      (1 to playerCount).map(id => Player(id)).toVector

    new Game(players, gameMode, playSet)
  }

  def run(): Option[Game] = {

    view.showPlayerCountPrompt()

    readNumberInput(2, 8).flatMap { count =>

      view.showSelectedPlayerCount(count)
      view.showPlaySetSelection()
      view.showPlaySets()

      readNumberInput(0, PlaySet.values.length - 1)
        .flatMap(PlaySet.fromId)
        .map { playSet =>

          view.showSelectedPlaySet(playSet)

          createGame(
            playerCount = count,
            gameMode = GameMode.StandardDeck,
            playSet = Some(playSet)
          )
        }
    }
  }
}
