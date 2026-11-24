
package dominion.model

import dominion.model.table.PlayTable
import dominion.util.PlaySet

import scala.util.Random

object GameFactory {

  def create(
              playerCount: Int,
              gameMode: GameMode,
              playSet: Option[PlaySet]
            ): Game = {

    require(
      playerCount >= 2 && playerCount <= 8,
      "A game requires between 2 and 8 players."
    )

    val players =
      Vector.tabulate(playerCount) { index =>
        Player(index + 1)
      }

    players.foreach { player =>
      val startingDeck =
        List.fill(7)(CardFactory.createCopper()) ++
          List.fill(3)(CardFactory.createEstate())

      player.addCardsToDeck(
        Random.shuffle(startingDeck)
      )

      player.drawCardsToHand(5)
    }

    new Game(
      players = players,
      gameMode = gameMode,
      playSet = playSet,
      playTable = PlayTable.standard(playerCount)
    )
  }
}
