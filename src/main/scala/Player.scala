class Player(val id: Int) {
  var perRoundActions: Int = 1
  var perRoundMoney: Int = 0
  var perRoundBuys: Int = 1

  private val deck = new Deck
  private val discardPile = new DiscardPile
}