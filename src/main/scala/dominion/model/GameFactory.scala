package dominion.model

import dominion.model.playset.{PlaySetCatalog, PlaySetDefinition}
import dominion.model.table.PlayTableFactory
import dominion.util.PlaySet

import scala.util.Random

object GameFactory {

  def create(
    playerCount: Int,
    gameMode: GameMode,
    playSet: Option[PlaySet]
  ): Game = {
    val selectedPlaySet = playSet.getOrElse(PlaySet.FirstGame)
    val definition = PlaySetCatalog.definition(selectedPlaySet)
    createWithDefinition(playerCount, gameMode, Some(selectedPlaySet), definition)
  }

  /**
   * Creates a game with a manually assembled, validated PlaySetDefinition.
   * The enum identifier is optional because custom presets need not be built-in.
   */
  def createWithDefinition(
    playerCount: Int,
    gameMode: GameMode,
    playSet: Option[PlaySet],
    definition: PlaySetDefinition
  ): Game = {
    require(playerCount >= 2 && playerCount <= 8,
      "A game requires between 2 and 8 players.")

    val players = Vector.tabulate(playerCount)(index => Player(index + 1))

    players.foreach { player =>
      val startingDeck =
        List.fill(7)(CardFactory.createCopper()) ++
          List.fill(3)(CardFactory.createEstate())
      player.addCardsToDeck(Random.shuffle(startingDeck))
      player.drawCardsToHand(5)
    }

    new Game(
      players = players,
      gameMode = gameMode,
      playSet = playSet,
      playTable = PlayTableFactory.create(playerCount, definition)
    )
  }
}
