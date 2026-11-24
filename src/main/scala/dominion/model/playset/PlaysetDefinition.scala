package dominion.model.playset

import dominion.model.Card
import dominion.util.PlaySet

/**
 * Beschreibt ein konkretes Kartenset.
 *
 * Die Implementierung liefert neue Karteninstanzen,
 * damit verschiedene Partien keine Kartenobjekte teilen.
 */
trait PlaySetDefinition:

  def playSet: PlaySet

  def kingdomCards: Vector[() => Card]

  def createKingdomCards: Vector[Card] =
    kingdomCards.map(create => create())

  requireKingdomCardCount()

  protected def requireKingdomCardCount(): Unit =
    require(
      kingdomCards.size == 10,
      s"${playSet.displayName} muss genau 10 Königreichkarten enthalten."
    )