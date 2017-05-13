def abs(x: Double): Double = if (x < 0) -x else x

def sqrt(x: Double): Double = {
  def sqrtIter(guess: Double): Double =
    if (goodEnough(guess)) guess
    else sqrtIter(improveGuess(guess))
  def goodEnough(guess: Double): Boolean = {
    abs(guess * guess - x) / x < 0.001
  }
  def improveGuess(guess: Double) = (guess + (x / guess)) / 2

  sqrtIter(1)
}




sqrt(4)

sqrt(1e-6)

sqrt(1e30)

1e-3
