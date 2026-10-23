class Player(val id: Int) {
  val perRoundActions: Int = 1
  val perRoundMoney: Int = 0
  val perRoundBuys: Int = 1

  private val deck = new Deck
  private val discardPile = new DiscardPile
}