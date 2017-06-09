package Week1

/**
  * Created by Oliver Coulson on 13/05/2017.
  */
/**
  * Created by Oliver Coulson on 13/05/2017.
  */
object Main extends App {

  def countChange(money: Int, coins: List[Int]): Int = countChangeRec(money, coins)

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

    def helper(remaining: Int, remainingCoins: List[Int]): Int = {
      if (remainingCoins.isEmpty) 0
      else {
        val diff = remaining - remainingCoins.head
        if (diff == 0) 1 + helper(remaining, remainingCoins.tail)
        else {
          if (diff > 0)
            helper(diff, remainingCoins) + helper(remaining, remainingCoins.tail)
          else {
            helper(remaining, remainingCoins.tail)
          }
        }
      }
    }
    val sortedCoins = coins.sortWith(_ > _)
    helper(money, sortedCoins)

  }

  println(countChange(100, List(100, 50, 20, 10, 5, 2,1)))
  println(countChange(4, List(5,2,1)))
  println(countChange(4, List(1)))

}

