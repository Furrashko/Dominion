package dominion.view

import dominion.controller.InitGameEvent
import dominion.model.playset.PlaySetCatalog
import dominion.util.Observer

class InitGameView(printMessage: String => Unit) extends Observer[InitGameEvent] {
  override def update(event: InitGameEvent): Unit = event match {
    case InitGameEvent.ShowPlayerCountPrompt =>
      printMessage("Number of Players:")
    case InitGameEvent.PlayerCountSelected(count) =>
      printMessage(s"Number of Players selected: $count\n")
    case InitGameEvent.ShowPlaySetSelection =>
      printMessage("Select Playset:")
    case InitGameEvent.ShowPlaySets =>
      showPlaySets()
    case InitGameEvent.PlaySetSelected(playSet) =>
      printMessage(s"Playset selected: ${playSet.displayName}\n")
    case InitGameEvent.InvalidInput(min, max) =>
      printMessage(s"Invalid input! Enter an integer between $min and $max.\n")
    case InitGameEvent.InvalidPlaySetInput(validIds) =>
      val allowed = if (validIds.isEmpty) "none" else validIds.mkString(", ")
      printMessage(s"Invalid PlaySet. Available PlaySet IDs: $allowed\n")
  }

  private def showPlaySets(): Unit = {
    val available = PlaySetCatalog.availablePlaySets
    if (available.isEmpty) printMessage("No complete PlaySets are currently available.")
    else available.foreach(playSet => printMessage(s"${playSet.id}: ${playSet.displayName}"))
  }
}
