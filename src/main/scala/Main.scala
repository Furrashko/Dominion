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

    parseNumberOfPlayers(readInput()) match {
      case Some(number) =>
        number

      case None =>
        printMessage("Invalid input! Enter an integer between 2 and 8.")
        loop()
    }
  }

  loop()
}

// $COVERAGE-OFF$
@main def main(): Unit = {
  println("Dominion")

  val numberOfPlayers = readNumberOfPlayers(
    () => scala.io.StdIn.readLine(),
    message => println(message)
  )

  println(s"Number of Players: $numberOfPlayers")

  val game = new Game(numberOfPlayers)

  //Stuff
}
// $COVERAGE-ON$