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
          s"Invalid input! Enter an integer between $min and $max.\n"
        )
        loop()
    }
  }

  loop()
}

enum PlaySet(val id: Int, val displayName: String) {
  case FirstGame extends PlaySet(0, "First Game (Base)")
  case SizeDistortion extends PlaySet(1, "Size Distortion (Base)")
  case DeckTop extends PlaySet(2, "Deck Top (Base)")
  case SleightOfHand extends PlaySet(3, "Sleight of Hand (Base)")
  case Improvements extends PlaySet(4, "Improvements (Base)")
  case SilverAndGold extends PlaySet(5, "Silver & Gold (Base)")
}

object PlaySet {
  def fromId(id: Int): Option[PlaySet] =
    PlaySet.values.find(_.id == id)
}

class InitGame(
                readInput: () => String,
                printMessage: String => Unit
              ) {
  println("Number of Players:")
  private val playerCount =
    readNumberInput(readInput, printMessage, 2, 8)

  println(s"Number of Players selected: $playerCount\n")

  println("Select Playset:")
  PlaySet.values.foreach { playSet =>
    println(s"${playSet.id}: ${playSet.displayName}")
  }

  private val playSetId =
    readNumberInput(readInput, printMessage, 0, PlaySet.values.length - 1)

  private val playSet =
    PlaySet.fromId(playSetId).get

  def selectedPlaySet: PlaySet = playSet

  println(s"Playset selected: ${playSet.displayName}\n")

  private val players: Vector[Player] =
    (1 to playerCount).map(id => Player(id)).toVector

  private var currentPlayerIndex: Int = 0

  def currentPlayer: Player =
    players(currentPlayerIndex)

  def nextTurn(): Unit = {
    currentPlayerIndex =
      (currentPlayerIndex + 1) % players.size
  }
}
