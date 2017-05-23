package Week3

/**
  * Created by Oliver Coulson on 23/05/2017.
  */
abstract class IntSet {
  def contains(x: Int): Boolean
  def incl(x: Int): IntSet
  def union(that: IntSet): IntSet
}

object Empty extends IntSet {
  def contains(x: Int) = false
  def incl(x: Int) = new NonEmpty(x, Empty, Empty)
  def union(that: IntSet): IntSet = that
}


class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet{
  def contains(x: Int) = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }

  def incl(x: Int) = {
    if (x < elem) new NonEmpty(elem, left incl x, right )
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this
  }

  def union(that: IntSet): IntSet = ((left union right) union that) incl elem

}