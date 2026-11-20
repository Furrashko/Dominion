
package dominion.model

object CardFactory {

  def createEstate(): VictoryCard =
    new VictoryCard("Estate", 2, 1)

  def createDuchy(): VictoryCard =
    new VictoryCard("Duchy", 5, 3)

  def createProvince(): VictoryCard =
    new VictoryCard("Province", 8, 6)

  def createCopper(): TreasureCard =
    new TreasureCard("Copper", 0, 1)

  def createSilver(): TreasureCard =
    new TreasureCard("Silver", 3, 2)

  def createGold(): TreasureCard =
    new TreasureCard("Gold", 6, 3)

  private val creators: Map[String, () => Card] = Map(
    "Estate"   -> (() => createEstate()),
    "Duchy"    -> (() => createDuchy()),
    "Province" -> (() => createProvince()),
    "Copper"   -> (() => createCopper()),
    "Silver"   -> (() => createSilver()),
    "Gold"     -> (() => createGold())
  )

  def create(name: String): Option[Card] =
    creators.get(name).map(_())

  def apply(name: String): Card =
    create(name).getOrElse {
      throw new IllegalArgumentException(
        s"Unknown card: $name"
      )
    }

  def createAll(names: Iterable[String]): Option[Vector[Card]] =
    names.foldLeft(Option(Vector.empty[Card])) {
      (result, name) =>
        for {
          cards <- result
          card <- create(name)
        } yield cards :+ card
    }

  def availableNames: Vector[String] =
    creators.keys.toVector.sorted
}
