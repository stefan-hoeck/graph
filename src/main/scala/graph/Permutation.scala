package graph

import collection.immutable.BitSet
import spire.syntax.cfor
import scalaz.syntax.monoid._

/** Represents a permutation of a sequence of values (which could be
  * the nodes of a graph)
  *
  * Permutations form a group under composition
  */
sealed trait Permutation {
  def apply(i: Int): Int

  /** Returns the permutation coming from first applying `that` and
    * then `this` permutation
    */
  def compose(that: Permutation): Permutation

  def inverse: Permutation

  def orbits: Orbits

  final def mapEdge(e: Edge): Edge = Edge(apply(e.a), apply(e.b))

  final def mapGraph(g: Graph): Graph = Graph(g.order, g.edges map mapEdge)
}

object Permutation {
  def apply(p: Array[Int]): Permutation = new Impl(p.clone())

  /** Returns an `Iterator` that generates all permutations
    * of a given size `n`.
    *
    * The number of elements in the `Iterator` is n!
    */
  def ofSize(n: Int): Iterator[Permutation] =
    (0 until n permutations) map { is ⇒ Permutation(is.toArray) }

  implicit val Group = new spire.algebra.Group[Permutation] {
    def inverse(p: Permutation) = p.inverse
    def id: Permutation = Ident
    def op(a: Permutation, b: Permutation) = a compose b
  }

  implicit val Monoid = new scalaz.Monoid[Permutation] {
    val zero: Permutation = Ident
    def append(a: Permutation, b: ⇒ Permutation) = a compose b
  }

  

  /**** Implementing classes ****/

  private case class Impl(p: Array[Int]) extends Permutation {
    def apply(i: Int): Int = if (i < p.size) p(i) else i

    def compose(that: Permutation): Permutation = that match {
      case Ident   ⇒ this
      case Impl(q) ⇒ new Impl(
        Array.tabulate(p.size max q.size){ i ⇒ this apply that(i) }
      )
    }

    def inverse: Permutation = {
      val a = Array.fill(p.size)(0)

      cfor(0)(_ < p.size, _ + 1) { i ⇒ a(apply(i)) = i }

      new Impl(a)
    }

    override def toString = {
      def ids = p mkString ", "

      s"($ids)"
    }

    def orbits: Orbits = {
      val res = Array.fill(p.size)(BitSet.empty)

      cfor(0)(_ < res.size, _ + 1){ i ⇒ 
        if (res(i).isEmpty) {
          var x = p(i)
          var bs: BitSet = BitSet(i)
          while(x != i) {
            bs += x
            x = p(x)
          }

          bs foreach { j ⇒ res(j) = bs }
        }
      }

      Orbits(res)
    }
  }

  private case object Ident extends Permutation {
    def apply(i: Int) = i
    def compose(that: Permutation) = that
    def inverse = this
    val orbits = ∅[Orbits]
    override def toString = "Identity Permutation"
  }
}

// vim: set ts=2 sw=2 et:
