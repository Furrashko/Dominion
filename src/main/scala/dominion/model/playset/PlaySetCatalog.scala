package dominion.model.playset

import dominion.util.PlaySet
import dominion.model.playset.presets.*

object PlaySetCatalog:

  private val definitions: Map[PlaySet, PlaySetDefinition] =
    Vector(
      FirstGame,
      SizeDistortion,
      DeckTop,
      SleightOfHand,
      Improvements,
      SilverAndGold
    ).map(definition => definition.playSet -> definition).toMap

  def get(playSet: PlaySet): Option[PlaySetDefinition] =
    definitions.get(playSet)

  def available: Vector[PlaySetDefinition] =
    definitions.values.toVector.sortBy(_.playSet.id)