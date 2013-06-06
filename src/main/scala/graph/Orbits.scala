package graph

import collection.immutable.BitSet
import spire.syntax.cfor

/** The orbits of a graph's automorphism group
  *
  * If there is an automorphism connecting two vertices
  * in a graph, the two vertices are in the same orbit.
  * An entry at index i points to a `BitSet` of indices
  * containing i and all other vertices of i's orbit
  *
  * Orbits are needed to prune the search tree when
  * finding a canonical labeling, but they can also
  * be of interest in other applications for instance
  * when finding chiral centers in a Molecule
  */
sealed trait Orbits {
  def apply(i: Int): BitSet
  def merge(that: Orbits): Orbits
}

object Orbits {
  private[graph] def apply(bs: Array[BitSet]): Orbits = Impl(bs)

  implicit val OrbitsMonoid = new scalaz.Monoid[Orbits] {
    val zero: Orbits = Ident
    def append(a: Orbits, b: ⇒ Orbits) = a merge b
  }

  private case class Impl(os: Array[BitSet]) extends Orbits {
    def apply(i: Int) = if (i < os.size) os(i) else BitSet(i)

    def merge(that: Orbits) = that match {
      case Ident ⇒ this
      case Impl(os2) ⇒ {
        val size = os.size max os2.size
        val res = Array.tabulate(size){ i ⇒ this(i) | that(i) }
        Impl(res)
      }
    }
  }

  private case object Ident extends Orbits {
    def apply(i: Int) = BitSet(i)
    def merge(that: Orbits) = that
  }
}

// vim: set ts=2 sw=2 et:
