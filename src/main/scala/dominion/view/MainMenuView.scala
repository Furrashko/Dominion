package dominion.view

class MainMenuView(
                    printMessage: String => Unit
                  ) {

  def showMenu(): Unit = {
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

  def showMessage(message: String): Unit =
    printMessage(message)
}