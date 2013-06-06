package graph

import collection.immutable.BitSet
import org.scalacheck._, Prop._
import scalaz._, Scalaz._

object OrbitsTest extends Properties("Orbits") {
  import PermutationTest._

  property("permutationToOrbits_isPartition") = forAll { p: Permutation ⇒ 
    val orbits = p.orbits
    val bs: BitSet = BitSet(0 to 6 flatMap orbits.apply: _*)
    bs == BitSet(0 until 7: _*)
  }

  property("permutationToOrbits_connections") = forAll { p: Permutation ⇒ 
    val orbits = p.orbits
    List.range(0, 7) ∀ { i ⇒ orbits(i)(p(i)) }
  }

  property("mergeOrbits_isPartition") = forAll {
    p: (Permutation, Permutation) ⇒ 
    val (p1, p2) = p
    val o1 = p1.orbits
    val o2 = p2.orbits
    val merged = o1 merge o2
    val bs: BitSet = BitSet(0 to 6 flatMap merged.apply: _*)

    bs == BitSet(0 until 7: _*)
  }

  property("mergeOrbits_connections") = forAll {
    p: (Permutation, Permutation) ⇒ 
    val (p1, p2) = p
    val o1 = p1.orbits
    val o2 = p2.orbits
    val merged = o1 merge o2

    List.range(0, 7) ∀ { i ⇒ 
      merged(i)(p1(i)) && merged(i)(p2(i))
    }
  }
}

// vim: set ts=2 sw=2 et:
