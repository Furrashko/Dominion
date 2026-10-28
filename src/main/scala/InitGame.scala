import scala.annotation.tailrec

def parseNumberOfPlayers(input: String): Option[Int] =
  input.toIntOption.filter(number => number >= 2 && number <= 8)

def readNumberOfPlayers(
                         readInput: () => String,
                         printMessage: String => Unit
                       ): Int = {

  @tailrec
  def loop(): Int = {
    printMessage("Enter number of Players:")

    Option(readInput()).flatMap(parseNumberOfPlayers) match {
      case Some(number) =>
        number

      case None =>
        printMessage("Invalid input! Enter an integer between 2 and 8.")
        loop()
    }
  }

  loop()
}

class InitGame(
                readInput: () => String, 
                printMessage: String => Unit) {
  private val playerCount = readNumberOfPlayers(readInput, printMessage)

  private val players: Vector[Player] =
    (1 to playerCount).map(id => new Player(id)).toVector

  private var currentPlayerIndex: Int = 0

  def currentPlayer: Player =
    players(currentPlayerIndex)

  def nextTurn(): Unit = {
    currentPlayerIndex =
      (currentPlayerIndex + 1) % players.size
  }
}