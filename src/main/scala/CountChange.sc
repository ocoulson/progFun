def countChange(money: Int, coins: List[Int]): Int = {

  def helper(remaining: Int, remainingCoins: List[Int], ways: Int): Int ={
    if (remainingCoins.isEmpty) ways
    else {
      if (remaining - remainingCoins.head <= 0)
        helper(remaining, remainingCoins.tail, ways+1)
      else helper(remaining - remainingCoins.head, remainingCoins, ways)
    }

  }

  val sorted = coins.sortWith(_ > _)
  helper(money, sorted, 0)

}

countChange(4, List(2,1))