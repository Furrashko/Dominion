import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class MainSpec extends AnyWordSpec with Matchers {

  "The main function" should {

    "run without errors" in {
      noException should be thrownBy {
        main()
      }
    }
  }
}