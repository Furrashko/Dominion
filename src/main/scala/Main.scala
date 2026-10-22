class Player {
  var per_round_actions = 1
  var per_round_money = 0
  var per_round_buys = 1
}

@main def main(): Unit = {
  println("Dominion")

  val player = new Player()
  println(player.per_round_actions)
}
