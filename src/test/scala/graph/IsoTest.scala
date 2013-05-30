package graph

import org.scalacheck._, Prop._
import scalaz._, Scalaz._

object IsoTest extends Properties("iso") {
  import iso._
  import test.samples._

  implicit val CellEqual: Equal[Cell] = Equal.equalA

  property("shatter_linear_permutation") = {
    def testPerm(g: Graph): Boolean = {
      //the first and last vertice of a linear graph have degree 1
      //all others have degree 2
      val exp = List(0, g.order - 1) ::: List.range(1, g.order - 1)

      exp ≟ shatterOnePermutation(g)
    }

    chains100.tail ∀ testPerm
  }

  property("shatter_linear_cells") = {
    def testCells(g: Graph): Boolean = {
      val iniC = Cell(0, 2)
      val restC = Cell(2, g.order - 2)
      val exp = iniC :: iniC :: List.fill(g.order - 2)(restC)

      exp ≟ shatterOneCells(g)
    }

    chains100.tail ∀ testCells
  }

  property("shatter_cycle_permutation") = {
    def testPerm(g: Graph): Boolean =
      (0 until g.order).toList ≟ shatterOnePermutation(g)

    rings100.tail ∀ testPerm
  }

  property("shatter_cycle_cells") = {
    def testCells(g: Graph): Boolean =
      List.fill(g.order)(Cell(0, g.order)) ≟ shatterOneCells(g)

    rings100.tail ∀ testCells
  }

  def shatterOnePermutation(g: Graph): List[Int] =
    shatterOne(g).p.toList

  def shatterOneCells(g: Graph): List[Cell] =
    shatterOne(g).c.toList

  def shatterOne(g: Graph): Iso = {
    val res = new Iso(g)
    val c = Cell(0, g.order)
    res.shatter(c, c)
    res
  }
}

// vim: set ts=2 sw=2 et:
