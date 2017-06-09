package Week4.Lists

/**
  * Created by Oliver Coulson on 04/06/2017.
  */

object Main extends App {

  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => {
      if (x < y) x :: xs
      else y :: insert(x, ys)
    }
  }
}