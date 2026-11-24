package dominion.controller

import scala.annotation.tailrec

import dominion.model.*
import dominion.model.playset.PlaySetCatalog
import dominion.util.{Observable, PlaySet}

enum InitGameEvent {
  case ShowPlayerCountPrompt
  case PlayerCountSelected(count: Int)
  case ShowPlaySetSelection
  case ShowPlaySets
  case PlaySetSelected(playSet: PlaySet)
  case InvalidInput(min: Int, max: Int)
  case InvalidPlaySetInput(validIds: Vector[Int])
}

object InitGameController {
  def parseNumberInput(input: String, min: Int, max: Int): Option[Int] =
    input.toIntOption.filter(number => number >= min && number <= max)
}

class InitGameController(readInput: () => String) extends Observable[InitGameEvent] {

  private def readNumberInput(min: Int, max: Int): Option[Int] = {
    @tailrec
    def loop(): Option[Int] =
      Option(readInput()) match {
        case None => None
        case Some(input) =>
          InitGameController.parseNumberInput(input, min, max) match {
            case some @ Some(_) => some
            case None =>
              notifyObservers(InitGameEvent.InvalidInput(min, max))
              loop()
          }
      }
    loop()
  }

  private def readAvailablePlaySet(): Option[PlaySet] = {
    val available = PlaySetCatalog.availablePlaySets
    val validIds = available.map(_.id)

    @tailrec
    def loop(): Option[PlaySet] =
      Option(readInput()) match {
        case None => None
        case Some(input) =>
          input.toIntOption.flatMap(id => available.find(_.id == id)) match {
            case some @ Some(_) => some
            case None =>
              notifyObservers(InitGameEvent.InvalidPlaySetInput(validIds))
              loop()
          }
      }
    loop()
  }

  def createGame(playerCount: Int, gameMode: GameMode, playSet: Option[PlaySet]): Game =
    GameFactory.create(playerCount, gameMode, playSet)

  def run(): Option[Game] = {
    notifyObservers(InitGameEvent.ShowPlayerCountPrompt)
    readNumberInput(2, 8).flatMap { count =>
      notifyObservers(InitGameEvent.PlayerCountSelected(count))
      notifyObservers(InitGameEvent.ShowPlaySetSelection)
      notifyObservers(InitGameEvent.ShowPlaySets)

      readAvailablePlaySet().map { playSet =>
        notifyObservers(InitGameEvent.PlaySetSelected(playSet))
        createGame(count, GameMode.StandardDeck, Some(playSet))
      }
    }
  }
}
