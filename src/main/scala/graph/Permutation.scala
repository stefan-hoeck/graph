package graph

import spire.syntax.cfor

/** Represents a permutation of a sequence of values (which could be
  * the nodes of a graph)
  *
  * Permutations form a group under composition
  */
//@TODO: Possible performance boost: Do not copy array at every
//combination but delegate to combined permutations. Keep track
//of depth of this 'permutation tree' and at a given depth create
//a new array to memoize the results.
sealed trait Permutation {
  def apply(i: Int): Int

  /** Returns the permutation coming from first applying `that` and
    * then `this` permutation
    */
  def compose(that: Permutation): Permutation

  def inverse: Permutation

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
  }

  private case object Ident extends Permutation {
    def apply(i: Int) = i
    def compose(that: Permutation) = that
    def inverse = this
    override def toString = "Identity Permutation"
  }
}

// vim: set ts=2 sw=2 et:
