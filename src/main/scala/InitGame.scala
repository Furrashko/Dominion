import scala.annotation.tailrec

def parseNumberInput(
                      input: String,
                      min: Int,
                      max: Int
                    ): Option[Int] =
  input.toIntOption.filter(number => number >= min && number <= max)

def readNumberInput(
                     readInput: () => String,
                     printMessage: String => Unit,
                     min: Int,
                     max: Int
                   ): Int = {

  @tailrec
  def loop(): Int = {
    printMessage(s"Enter an integer between $min and $max:")

    Option(readInput()).flatMap(input =>
      parseNumberInput(input, min, max)
    ) match {
      case Some(number) =>
        number

      case None =>
        printMessage(
          s"Invalid input! Enter an integer between $min and $max."
        )
        loop()
    }
  }

  loop()
}

class InitGame(
                readInput: () => String, 
                printMessage: String => Unit) {
  private val playerCount = readNumberInput(readInput, printMessage, 2, 8)

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