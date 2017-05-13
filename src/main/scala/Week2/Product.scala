package Week2

/**
  * Created by Oliver Coulson on 13/05/2017.
  */
object Product extends App{

  def mapReduce(start: Int, h: (Int, Int) => Int, f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) start
    else h(f(a), mapReduce(start , h, f)(a+1, b))
  }
  def sum(f: Int => Int)(a: Int, b: Int):Int = {
    mapReduce(0, (x, y) => x + y, f)(a,b)
  }

  def product(f: Int => Int)(a:Int, b:Int): Int = {
    mapReduce(1, (x, y) => x * y, f)(a,b)
  }


  def factorial(n: Int): Int =
    product(x => x)(1, n)

  println(product(x => x)(1,3))

  println(factorial(6))
}
