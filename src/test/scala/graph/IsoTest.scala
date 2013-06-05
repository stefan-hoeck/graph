package graph

import collection.immutable.BitSet
import org.scalacheck._, Prop._
import scalaz._, Scalaz._

object IsoTest extends Properties("iso") {
  import PermutationTest._
  import iso2._
  import test.samples._

  implicit val CellEqual: Equal[Cell] = Equal.equalA

  property("refine_linear_permutation") = {
    def testPerm(g: Graph): Boolean = {
      val isEven = g.order % 2 == 0
      val center = (g.order - 1) / 2
      def distanceFromCenter(i: Int) = 
        if (isEven && i > center) (center - i).abs - 1
        else (center - i).abs

      val exp = List(0, g.order - 1) :::
                List.range(1, g.order - 1).sortBy(distanceFromCenter)

      val res = refinePermutation(g)

      exp ≟ res
    }

    chains100.tail ∀ testPerm
  }

  property("refine_linear_cells") = {
    def testCells(g: Graph): Boolean = {
      val cells = refineCells(g)
      val center = if (g.order % 2 == 0) -1 else g.order / 2

      def test(i: Int) = {
        val c = cells(i)
        if (i == center) c.size == 1 else c.size == 2
      }

      (0 until g.order).toList ∀ test
    }

    chains100.tail ∀ testCells
  }

  property("refine_cycle_permutation") = {
    def testPerm(g: Graph): Boolean =
      (0 until g.order).toList ≟ refinePermutation(g)

    rings100.tail ∀ testPerm
  }

  property("refine_cycle_cells") = {
    def testCells(g: Graph): Boolean =
      List.fill(g.order)(Cell(0, g.order)) ≟ refineCells(g)

    rings100.tail ∀ testCells
  }

  property("permutationToOrbits_isPartition") = forAll { p: Permutation ⇒ 
    val orbits = iso2 permutationToOrbits(p, 7)
    orbits.fold(BitSet.empty)(_ | _) == BitSet(0 until 7: _*)
  }

  property("permutationToOrbits_connections") = forAll { p: Permutation ⇒ 
    val orbits = iso2 permutationToOrbits(p, 7)
    List.range(0, 7) ∀ { i ⇒ orbits(i)(p(i)) }
  }

  property("mergeOrbits_isPartition") = forAll {
    p: (Permutation, Permutation) ⇒ 
    val (p1, p2) = p
    val o1 = iso2 permutationToOrbits(p1, 7)
    val o2 = iso2 permutationToOrbits(p2, 7)
    val merged = mergeOrbits(o1, o2)

    merged.fold(BitSet.empty)(_ | _) == BitSet(0 until 7: _*)
  }

  property("mergeOrbits_connections") = forAll {
    p: (Permutation, Permutation) ⇒ 
    val (p1, p2) = p
    val o1 = iso2 permutationToOrbits(p1, 7)
    val o2 = iso2 permutationToOrbits(p2, 7)
    val merged = mergeOrbits(o1, o2)

    List.range(0, 7) ∀ { i ⇒ 
      merged(i)(p1(i)) && merged(i)(p2(i))
    }
  }

  property("solve_isCanonical") = {
    val order = 5
    val gs = test.samples allOfOrder order
    val ps = Permutation ofSize order toList

    gs ∀ { testCanonical(_, ps) }
  }

  def testCanonical(g: Graph, ps: List[Permutation]): Boolean = {
    def canonicalSet(g: Graph) = (iso2 solve g)._1 mapGraph g edges

    val sets = ps map { p ⇒ canonicalSet(p mapGraph g) } toSet

    if (sets.size > 1) {
      println(g)
      println(sets mkString "\n")
    }

    sets.size ≟ 1
  }

  def refinePermutation(g: Graph): List[Int] = refine(g)._1

  def refineCells(g: Graph): List[Cell] = refine(g)._2

  def refine(g: Graph): (List[Int], List[Cell]) = iso2 refine g
}

// vim: set ts=2 sw=2 et:
