//Fixed point of a function is a value x such that f(x) = x
import math.abs
def f(x: Int) = 1 + x/2

def fixedPoint(f: Double => Double)(initialGuess: Double) = {
  def iterate(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(guess, next, 0.0001)) next
    else iterate(next)
  }
  iterate(initialGuess)
}

def isCloseEnough(a: Double, b: Double, tolerance: Double): Boolean ={
  abs((a - b) / a) / a < tolerance
}


fixedPoint((x) => 1 + x/2)(1)


def sqrt(x: Double) = fixedPoint(y => ((y + x) / y) / 2)(1)

def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

def sqrt2(x: Double) =
  fixedPoint(averageDamp(y => x/y))(1)

sqrt2(2)