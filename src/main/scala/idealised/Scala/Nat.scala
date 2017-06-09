package idealised.Scala

/**
  * Created by Oliver Coulson on 04/06/2017.
  */
abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}


object Zero extends Nat {
  def isZero: Boolean = true
  def predecessor = throw new Error("Zero.predecessor")
  def + (that: Nat) = that
  def - (that: Nat) = if (that.isZero) this else throw new Error("Zero - Non-zero")
}
class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false

  override def predecessor: Nat = n

  override def +(that: Nat): Nat = new Succ(n + that)

  override def -(that: Nat): Nat = if (that.isZero) this else n - that.predecessor
}
