
package dominion.view

import dominion.controller.InitGameEvent
import dominion.util.{Observer, PlaySet}

class InitGameView(
                    printMessage: String => Unit
                  ) extends Observer[InitGameEvent] {

  override def update(event: InitGameEvent): Unit = {

    event match {

      case InitGameEvent.ShowPlayerCountPrompt =>
        showPlayerCountPrompt()

      case InitGameEvent.PlayerCountSelected(count) =>
        showSelectedPlayerCount(count)

      case InitGameEvent.ShowPlaySetSelection =>
        showPlaySetSelection()

      case InitGameEvent.ShowPlaySets =>
        showPlaySets()

      case InitGameEvent.PlaySetSelected(playSet) =>
        showSelectedPlaySet(playSet)

      case InitGameEvent.InvalidInput(min, max) =>
        showInvalidInput(min, max)
    }
  }

  private def showPlayerCountPrompt(): Unit =
    printMessage("Number of Players:")

  private def showSelectedPlayerCount(count: Int): Unit =
    printMessage(s"Number of Players selected: $count\n")

  private def showPlaySetSelection(): Unit =
    printMessage("Select Playset:")

  private def showPlaySets(): Unit =
    PlaySet.values.foreach { playSet =>
      printMessage(s"${playSet.id}: ${playSet.displayName}")
    }

  private def showSelectedPlaySet(playSet: PlaySet): Unit =
    printMessage(s"Playset selected: ${playSet.displayName}\n")

  private def showInvalidInput(min: Int, max: Int): Unit =
    printMessage(
      s"Invalid input! Enter an integer between $min and $max.\n"
    )
}
