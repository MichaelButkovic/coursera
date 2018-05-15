abstract class IntSet {
  def incl(x: Int): IntSet

  def contains(x: Int): Boolean

  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  override def toString = "."
  def contains(x: Int): Boolean = false

  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

  def union(other: IntSet): IntSet = other

}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

  override def toString = "{" + left + elem + right + "}"

  def contains(x: Int): Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }

  def incl(x: Int): IntSet = {
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this
  }

  def union(other: IntSet): IntSet = {
    ((left union right) union other) incl elem
  }
}

val t1 = new NonEmpty(3, Empty, Empty)
val t2 = t1 incl 4


def nth(n: Int, l: List[T]): T = {
  if (l.length - 1 > n) throw IndexOutOfBoundsException
  else l(n)
}