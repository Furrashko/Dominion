package dominion.util

enum PlaySet(val id: Int, val displayName: String):
  case FirstGame
    extends PlaySet(0, "First Game (Base)")

  case SizeDistortion
    extends PlaySet(1, "Size Distortion (Base)")

  case DeckTop
    extends PlaySet(2, "Deck Top (Base)")

  case SleightOfHand
    extends PlaySet(3, "Sleight of Hand (Base)")

  case Improvements
    extends PlaySet(4, "Improvements (Base)")

  case SilverAndGold
    extends PlaySet(5, "Silver & Gold (Base)")

object PlaySet:
  def fromId(id: Int): Option[PlaySet] =
    PlaySet.values.find(_.id == id)