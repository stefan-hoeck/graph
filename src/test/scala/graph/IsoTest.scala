package graph

import org.scalacheck._, Prop._
import scalaz._, Scalaz._

object IsoTest extends Properties("iso") {
  import iso._
  import test.samples._

  implicit val CellEqual: Equal[Cell] = Equal.equalA

  property("refine_linear_permutation") = {
    def testPerm(g: Graph): Boolean = {
      //the first and last vertice of a linear graph have degree 1
      //all others have degree 2
      val exp = List(0, g.order - 1) ::: List.range(1, g.order - 1)
      val res = refinePermutation(g)
      println(res)

      exp ≟ refinePermutation(g)
    }

    chains100.tail ∀ testPerm
  }

  property("refine_linear_cells") = {
    def testCells(g: Graph): Boolean = {
      val iniC = Cell(0, 2)
      val restC = Cell(2, g.order - 2)
      val exp = iniC :: iniC :: List.fill(g.order - 2)(restC)

      exp ≟ refineCells(g)
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

  def refinePermutation(g: Graph): List[Int] =
    refine(g).p.toList

  def refineCells(g: Graph): List[Cell] =
    refine(g).c.toList

  def refine(g: Graph): Iso = {
    val res = new Iso(g)
    res.refine()
    res
  }
}

// vim: set ts=2 sw=2 et:
