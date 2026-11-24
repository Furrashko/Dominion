package dominion.model.playset

import dominion.model.ActionCard
import dominion.model.actioncard.*
import dominion.util.PlaySet

/**
 * Registry for built-in presets. A PlaySet enum value is not considered
 * playable until it has a registered, valid definition.
 */
object PlaySetCatalog {

  private val definitions: Map[PlaySet, () => PlaySetDefinition] =
    Map(
      PlaySet.FirstGame -> (() => presets.FirstGame.definition)
    )

  def definition(playSet: PlaySet): PlaySetDefinition =
    definitions.get(playSet).map(_()).getOrElse {
      throw new IllegalArgumentException(
        s"PlaySet '${playSet.displayName}' is not implemented completely. " +
          "It must define exactly ten different ActionCards before it can be played."
      )
    }

  def availablePlaySets: Vector[PlaySet] =
    definitions.keys.toVector.sortBy(_.id)

  def customDefinition(
                        name: String,
                        actionCards: Seq[() => ActionCard]
                      ): PlaySetDefinition =
    PlaySetDefinition.custom(name, actionCards)
}
