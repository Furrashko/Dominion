
package dominion.model

enum TurnPhase {

  case Action
  case Buy
  case Cleanup

  def next: Option[TurnPhase] =
    this match {

      case Action =>
        Some(Buy)

      case Buy =>
        Some(Cleanup)

      case Cleanup =>
        None
    }
}
