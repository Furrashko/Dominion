import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

// Noch keine Tests für die Main funktion
// class MainSpec extends AnyWordSpec with Matchers {}

class PlayerSpec extends AnyWordSpec with Matchers {
  "A Player" should {
    "have the correct initial values" in {
      val player = new Player()

      player.per_round_actions shouldBe 1
      player.per_round_money shouldBe 0
      player.per_round_buys shouldBe 1
    }
  }
}