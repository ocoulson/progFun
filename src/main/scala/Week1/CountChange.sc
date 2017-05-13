def countChange(money: Int, coins: List[Int]): Int = countChangeTail(money, coins)
def countChangeTail(money: Int, coins: List[Int]): Int = {
  def helper(remaining: Int, remainingCoins: List[Int], combinations: Int): Int ={
    if (remainingCoins.isEmpty) combinations
    else {
      if (remaining - remainingCoins.head == 0)
        helper(remaining, remainingCoins.tail, combinations+1)
      else if (remaining - remainingCoins.head < 0)
        helper(remaining, remainingCoins.tail, combinations)
      else
        helper(remaining - remainingCoins.head, remainingCoins, combinations)
    }
  }
  val sortedCoins = coins.sortWith(_ > _)
  helper(money, sortedCoins, 0)
}
def countChangeRec(money: Int, coins: List[Int]): Int = {
  def helper(remaining: Int, remainingCoins: List[Int]): Int ={
    if (remainingCoins.isEmpty) 0
    else {
      if (remaining - remainingCoins.head == 0)
        1 + helper(remaining, remainingCoins.tail)
      else if (remaining - remainingCoins.head < 0)
        helper(remaining, remainingCoins.tail)
      else
        helper(remaining - remainingCoins.head, remainingCoins)
        helper(remaining, remainingCoins.tail)
    }
  }
  val sortedCoins = coins.sortWith(_ > _)
  helper(money, sortedCoins)

}

countChange(4, List(2,1))
countChange(4, List(5,2,1))
countChange(4, List(1))
