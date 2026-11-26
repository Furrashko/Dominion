package dominion.model

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TurnPhaseSpec extends AnyWordSpec with Matchers {

  "TurnPhase" should {

    "continue from Action with Buy" in {
      TurnPhase.Action.next shouldBe Some(TurnPhase.Buy)
    }

    "continue from Buy with Cleanup" in {
      TurnPhase.Buy.next shouldBe Some(TurnPhase.Cleanup)
    }

    "have no next phase after Cleanup" in {
      TurnPhase.Cleanup.next shouldBe None
    }
  }
}
