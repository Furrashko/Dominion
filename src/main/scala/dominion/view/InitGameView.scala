package dominion.view

import dominion.model.PlaySet

class InitGameView(printMessage: String => Unit) {

  def showPlayerCountPrompt(): Unit =
    printMessage("Number of Players:")

  def showSelectedPlayerCount(count: Int): Unit =
    printMessage(s"Number of Players selected: $count\n")

  def showPlaySetSelection(): Unit =
    printMessage("Select Playset:")

  def showPlaySets(): Unit =
    PlaySet.values.foreach { playSet =>
      printMessage(s"${playSet.id}: ${playSet.displayName}")
    }

  def showSelectedPlaySet(playSet: PlaySet): Unit =
    printMessage(s"Playset selected: ${playSet.displayName}\n")

  def showInvalidInput(min: Int, max: Int): Unit =
    printMessage(
      s"Invalid input! Enter an integer between $min and $max.\n"
    )
}