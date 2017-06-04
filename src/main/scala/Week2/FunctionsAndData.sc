class Rational(x: Int, y: Int) {

  require(y != 0, "denominator must not be 0")

  def this(x: Int) = this(x, 1)

  private def gcd(x: Int, y: Int): Int = if (y == 0) x else gcd(y, x % y)

  def numer: Int = x
  def denom: Int = y

  def + (that: Rational): Rational =
    new Rational(numer * that.denom + that.numer * denom, denom * that.denom)

  def - (that: Rational): Rational =
    this + -that

  def * (that: Rational): Rational =
    new Rational(numer * that.numer, denom * that.denom)

  def / (that: Rational): Rational =
    new Rational(numer * that.denom, denom * that.numer)

  def == (that: Rational): Boolean = numer * that.denom == denom * that.numer

  def unary_- : Rational = new Rational(- numer, denom)

  def < (that: Rational): Boolean = numer * that.denom < that.numer * denom

  def > (that: Rational): Boolean = numer * that.denom > that.numer * denom

  def <= (that: Rational): Boolean = this < that || equals(that)

  def >= (that: Rational): Boolean = this > that || equals(that)

  def asDouble: Double = numer / denom

  def max(that: Rational): Rational = if (this < that) that else this

  def min(that: Rational): Rational = if (this < that) this else that

  override def toString: String = {
    val g = gcd(numer, denom)
    (numer/g).toString + '/' + (denom/g).toString
  }
}
val first = new Rational(1,2)

val second = new Rational(1,4)

first * second


val x = new Rational(1,3)
val y = new Rational(5,7)
val z = new Rational(3,2)

x - y - z