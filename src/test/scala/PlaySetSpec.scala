
package dominion.model

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class PlaySetSpec extends AnyWordSpec with Matchers {

  "PlaySet" should {

    "contain all six playsets" in {
      PlaySet.values.length shouldBe 6
    }

    "have the correct IDs and names" in {
      PlaySet.FirstGame.id shouldBe 0
      PlaySet.FirstGame.displayName shouldBe "First Game (Base)"

      PlaySet.SizeDistortion.id shouldBe 1
      PlaySet.SizeDistortion.displayName shouldBe "Size Distortion (Base)"

      PlaySet.DeckTop.id shouldBe 2
      PlaySet.DeckTop.displayName shouldBe "Deck Top (Base)"

      PlaySet.SleightOfHand.id shouldBe 3
      PlaySet.SleightOfHand.displayName shouldBe "Sleight of Hand (Base)"

      PlaySet.Improvements.id shouldBe 4
      PlaySet.Improvements.displayName shouldBe "Improvements (Base)"

      PlaySet.SilverAndGold.id shouldBe 5
      PlaySet.SilverAndGold.displayName shouldBe "Silver & Gold (Base)"
    }

    "return the correct playset for every valid ID" in {
      PlaySet.fromId(0) shouldBe Some(PlaySet.FirstGame)
      PlaySet.fromId(1) shouldBe Some(PlaySet.SizeDistortion)
      PlaySet.fromId(2) shouldBe Some(PlaySet.DeckTop)
      PlaySet.fromId(3) shouldBe Some(PlaySet.SleightOfHand)
      PlaySet.fromId(4) shouldBe Some(PlaySet.Improvements)
      PlaySet.fromId(5) shouldBe Some(PlaySet.SilverAndGold)
    }

    "return None for invalid IDs" in {
      PlaySet.fromId(-1) shouldBe None
      PlaySet.fromId(6) shouldBe None
      PlaySet.fromId(100) shouldBe None
    }
  }
}
