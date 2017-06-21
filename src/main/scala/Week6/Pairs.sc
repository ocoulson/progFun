def isPrime(n: Int): Boolean = (2 until n) forall (i => n % i != 0)

val n = 7
(1 until n) flatMap(i => (1 until i) map(j => (i, j))) filter {case (x,y)=> isPrime(x+y)}

for{
  i <- 1 until n
  j <- 1 until i
  if isPrime(i+j)
}  yield (i, j)


def product(xs: List[Double], ys: List[Double]) =
  (for ((x,y) <- xs zip ys) yield x*y).sum

product(List(1, 1, 2), List(3, 4, 5))