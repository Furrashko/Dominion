
package dominion.controller

import scala.annotation.tailrec

import dominion.model.*
import dominion.util.{Observable, PlaySet}

enum InitGameEvent {
  case ShowPlayerCountPrompt
  case PlayerCountSelected(count: Int)
  case ShowPlaySetSelection
  case ShowPlaySets
  case PlaySetSelected(playSet: PlaySet)
  case InvalidInput(min: Int, max: Int)
}

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
                          readInput: () => String
                        ) extends Observable[InitGameEvent] {

  private def readNumberInput(
                               min: Int,
                               max: Int
                             ): Option[Int] = {

    @tailrec
    def loop(): Option[Int] = {
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
              notifyObservers(
                InitGameEvent.InvalidInput(min, max)
              )
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
                ): Game =
    GameFactory.create(
      playerCount,
      gameMode,
      playSet
    )

  def run(): Option[Game] = {

    notifyObservers(
      InitGameEvent.ShowPlayerCountPrompt
    )

    readNumberInput(2, 8).flatMap { count =>

      notifyObservers(
        InitGameEvent.PlayerCountSelected(count)
      )

      notifyObservers(
        InitGameEvent.ShowPlaySetSelection
      )

      notifyObservers(
        InitGameEvent.ShowPlaySets
      )

      readNumberInput(0, PlaySet.values.length - 1)
        .flatMap(PlaySet.fromId)
        .map { playSet =>

          notifyObservers(
            InitGameEvent.PlaySetSelected(playSet)
          )

          createGame(
            playerCount = count,
            gameMode = GameMode.StandardDeck,
            playSet = Some(playSet)
          )
        }
    }
  }
}
