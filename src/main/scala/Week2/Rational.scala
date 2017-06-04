package Week2

/**
  * Created by oliver on 19/05/17.
  */
class Rational(x: Int, y: Int) {
  private def gcd(x: Int, y: Int): Int = if (y == 0) x else gcd(y, x % y)
  private val g = gcd(x,y)
  def numer: Int = x / g
  def denom: Int = y / g

  def add(that: Rational): Rational =
    new Rational(numer * that.denom + that.numer * denom, denom * that.denom)

  def sub(that: Rational): Rational =
    new Rational(numer * that.denom - that.numer * denom, denom * that.denom)

  def mul(that: Rational): Rational =
    new Rational(numer * that.numer, denom * that.denom)

  def div(that: Rational): Rational =
    new Rational(numer * that.denom, denom * that.numer)

  def equals(that: Rational): Boolean = numer * that.denom == denom * that.numer

  def neg: Rational = new Rational(- numer, denom)

  def less(that: Rational): Boolean = numer * that.denom < that.numer * denom

  def greater(that: Rational): Boolean = numer * that.denom > that.numer * denom

  def lessOrEqual(that: Rational): Boolean = less(that) || equals(that)

  def greaterOrEqual(that: Rational): Boolean = greater(that) || equals(that)

  def asDouble: Double = numer / denom

  override def toString: String = numer.toString + '/' + denom.toString
}
