
package dominion.model

import dominion.util.PlaySet

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

    new Game(
      players,
      gameMode,
      playSet
    )
  }
}
