package graph

import org.scalacheck._, Prop._
import scalaz._, Scalaz._

object IsoTest extends Properties("iso") {
  import iso._
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

  def refinePermutation(g: Graph): List[Int] = refine(g)._1

  def refineCells(g: Graph): List[Cell] = refine(g)._2

  def refine(g: Graph): (List[Int], List[Cell]) = iso refine g
}

// vim: set ts=2 sw=2 et:
