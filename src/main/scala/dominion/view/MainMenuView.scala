package dominion.view

import dominion.controller.MainMenuEvent
import dominion.util.Observer

class MainMenuView(
                    printMessage: String => Unit
                  ) extends Observer[MainMenuEvent] {

  override def update(event: MainMenuEvent): Unit = {
    event match {
      case MainMenuEvent.ShowMenu =>
        showMenu()

      case MainMenuEvent.ShowMessage(message) =>
        showMessage(message)
    }
  }

  private def showMenu(): Unit = {
    printMessage(
      """
        |-- Choose Game-mode: --
        |Type 0 for Cheat Mode.
        |Type 1 for Starter Deck.
        |Type "help" to get help.
        |Type "exit" to terminate the session.
        |""".stripMargin
    )
  }

  private def showMessage(message: String): Unit =
    printMessage(message)
}