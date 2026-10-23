
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PlayerSpec extends AnyWordSpec with Matchers {

  "A Player" should {

    "have the correct initial values" in {
      val player = Player(1)

      player.id shouldBe 1
      player.perRoundActions shouldBe 1
      player.perRoundMoney shouldBe 0
      player.perRoundBuys shouldBe 1
    }

    "allow changing per-round actions" in {
      val player = Player(1, perRoundActions = 3)

      player.perRoundActions shouldBe 3
    }

    "allow changing per-round money" in {
      val player = Player(1, perRoundMoney = 5)
      
      player.perRoundMoney shouldBe 5
    }

    "allow changing per-round buys" in {
      val player = Player(1, perRoundBuys = 2)

      player.perRoundBuys shouldBe 2
    }
  }
}
