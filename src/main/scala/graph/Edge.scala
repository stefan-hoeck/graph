package graph

import scalaz._, Scalaz._

/**
 * An edge in an unlabeled undirected graph
 *
 * The connected vertices are given by the integers
 * a and b.
 *
 * It is an invariant of this Edge-implementation that
 * for all edges b >= a
 *
 * @author Stefan Höck
 */
final class Edge private(val a: Int, val b: Int) extends Ordered[Edge] {
  /**
   * Returns true if this edge connects the given vertex v.
   */
  def connects(v: Int) =  v == a || v == b

  /**
   * Returns the neighbor of the given vertex v, if this
   * edge is adjacent to v. Otherwise returns None.
   */
  def neighborOf(v: Int): Option[Int] = v match {
    case `a` ⇒ Some(b)
    case `b` ⇒ Some(a)
    case _   ⇒ None
  }

  /**
   * Defines the natural ordering of edges.
   * The implementation first compares vertices b and then vertices a.
   */
  def compare(that: Edge) =
    if (b == that.b) a compare that.a else b compare that.b

  override val hashCode = a + b * 7919

  override def equals(that: Any) = that match {
    case e: Edge ⇒ e.a == a && e.b == b
    case _       ⇒ false
  }

  override def toString = s"$a~$b"
}

object Edge {

  /**
   * Returns an instance of class Edge connecting
   * vertices a and b
   */
  def apply(a: Int, b: Int): Edge = {
    require(a >= 0 && b >= 0 && a != b)

    if (a < 250 && b < 250) cache(a)(b)
    else create(a, b)
  }

  private def create(a: Int, b: Int): Edge = {
    if (a < b) new Edge(a, b) else new Edge (b, a)
  }

  implicit val EdgeOrder: Order[Edge] = 
    Order order ((a,b) ⇒ Ordering fromInt a.compare(b))

  private[this] val cache = Array.tabulate(250, 250)(create)

}

// vim: set ts=2 sw=2 et:
