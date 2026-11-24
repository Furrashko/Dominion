
package dominion.model

import dominion.model.playset.{PlaySetCatalog, PlaySetDefinition}
import dominion.util.PlaySet
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PlaySetDefinitionSpec extends AnyWordSpec with Matchers {

  private def factories: Vector[() => ActionCard] =
    Vector.tabulate(10) { index =>
      () => new ActionCard(s"Action $index", index) {}
    }

  "PlaySetDefinition" should {

    "create a definition with ten action cards" in {
      val definition = new PlaySetDefinition(
        "Test",
        factories
      )

      definition.name shouldBe "Test"
      definition.actionCardFactories should have size 10
    }

    "create action cards from its factories" in {
      val definition = new PlaySetDefinition(
        "Test",
        factories
      )

      val cards = definition.createActionCards

      cards should have size 10
      cards.map(_.name).distinct should have size 10
    }

    "return the action card names" in {
      val definition = new PlaySetDefinition(
        "Test",
        factories
      )

      definition.actionCardNames shouldBe
        (0 until 10).map(index => s"Action $index").toVector
    }

    "create fresh card instances on every call" in {
      val definition = new PlaySetDefinition(
        "Test",
        factories
      )

      val first = definition.createActionCards
      val second = definition.createActionCards

      first.zip(second).foreach { case (left, right) =>
        (left eq right) shouldBe false
      }
    }

    "reject an empty name" in {
      an[IllegalArgumentException] should be thrownBy {
        new PlaySetDefinition("", factories)
      }
    }

    "reject a whitespace-only name" in {
      an[IllegalArgumentException] should be thrownBy {
        new PlaySetDefinition("   ", factories)
      }
    }

    "reject fewer than ten factories" in {
      an[IllegalArgumentException] should be thrownBy {
        new PlaySetDefinition("Test", factories.take(9))
      }
    }

    "reject more than ten factories" in {
      an[IllegalArgumentException] should be thrownBy {
        new PlaySetDefinition(
          "Test",
          factories :+ (() => new ActionCard("Extra", 0) {})
        )
      }
    }

    "reject duplicate action card names" in {
      val duplicateFactories = Vector.fill(10)(
        () => new ActionCard("Duplicate", 0) {}
      )

      an[IllegalArgumentException] should be thrownBy {
        new PlaySetDefinition("Test", duplicateFactories)
      }
    }

    "allow custom definitions" in {
      val definition = PlaySetDefinition.custom(
        "Custom",
        factories
      )

      definition.name shouldBe "Custom"
      definition.createActionCards should have size 10
    }
  }

  "PlaySetCatalog" should {

    "provide the FirstGame definition" in {
      val definition =
        PlaySetCatalog.definition(PlaySet.FirstGame)

      definition.name shouldBe "First Game"
      definition.createActionCards should have size 10
    }

    "provide only implemented PlaySets" in {
      PlaySetCatalog.availablePlaySets shouldBe
        Vector(PlaySet.FirstGame)
    }

    "sort available PlaySets by identifier" in {
      val available = PlaySetCatalog.availablePlaySets

      available shouldBe available.sortBy(_.id)
    }

    "reject an unimplemented PlaySet" in {
      an[IllegalArgumentException] should be thrownBy {
        PlaySetCatalog.definition(PlaySet.SizeDistortion)
      }
    }

    "reject another unimplemented PlaySet" in {
      an[IllegalArgumentException] should be thrownBy {
        PlaySetCatalog.definition(PlaySet.SilverAndGold)
      }
    }

    "create custom definitions through the catalog" in {
      val definition = PlaySetCatalog.customDefinition(
        "Custom",
        factories
      )

      definition.name shouldBe "Custom"
      definition.actionCardNames should have size 10
    }
  }
}
