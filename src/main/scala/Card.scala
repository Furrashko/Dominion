
enum CardType:
    case Coin
    case Action
    case Territory

class Card(val name: String,
            val cost: Int,
            val deck: PlaySet,
            val cardType: CardType) {}
