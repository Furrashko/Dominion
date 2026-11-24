package dominion.model.playset.presets

import dominion.model.playset.PlaySetDefinition
import dominion.model.actioncard.*

object FirstGame {

  val definition: PlaySetDefinition =
    new PlaySetDefinition(
      name = "First Game",
      actionCardFactories = Vector(
        () => new Cellar,
        () => new Market,
        () => new Militia,
        () => new Mine,
        () => new Moat,
        () => new Remodel,
        () => new Smithy,
        () => new Village,
        () => new Workshop,
        () => new Woodcutter
      )
    )
}
