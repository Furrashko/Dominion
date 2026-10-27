case class Player(id: Int, perRoundActions: Int = 1, perRoundMoney: Int = 0, perRoundBuys: Int = 1) {
  
  private val deck = new Deck
  private val discardPile = new DiscardPile
}