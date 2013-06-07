package graph

import collection.immutable.BitSet
import org.scalacheck._, Prop._
import scalaz._, Scalaz._

object IsoTest extends Properties("iso") {
  import PermutationTest._
  import iso2._
  import test.samples._

  implicit val CellEqual: Equal[Cell] = Equal.equalA

  val sixes = test.samples allOfOrder 6

  //chains of size n should be refined to
  //the following numbering (c is the index of the central
  //vertex or the two central vertices):
  //(0, n-1, c, c-1, c+1, c-2, c+2)
  //this is because vertices are first sorted by degree and
  //then the large middle cell is shattered by the cell holding
  //the end vertices. Therefore, vertices closer to the ends
  //get a higher score
  property("refine_chain_permutation") = {
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

  //see comment for "refine_chain_permutation"
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

  //checks that refining the vertices of a cycle
  //does nothing, since all vertices are symmetric
  property("refine_cycle_permutation") = {
    def testPerm(g: Graph): Boolean =
      (0 until g.order).toList ≟ refinePermutation(g)

    rings100.tail ∀ testPerm
  }

  //checks that refining the vertices of a cycle
  //does nothing, since all vertices are symmetric
  property("refine_cycle_cells") = {
    def testCells(g: Graph): Boolean =
      List.fill(g.order)(Cell(0, g.order)) ≟ refineCells(g)

    rings100.tail ∀ testCells
  }

  property("initial_unscored") = {
    def test(g: Graph) = {
      val (m, c, os) = iniUnscored(g)

      iniPartitionUnscored(g, m) &&
      iniCellsDegree0Unscored(g, m) &&
      iniCellsNon0Unscored(g, m) &&
      iniPiUnscored(g, c, m) &&
      iniNonSUnscored(g, c, m) &&
      iniAlphaUnscored(g, c, m) &&
      iniOrbitsDeg0Unscored(g, os) &&
      iniOrbitsDegNon0Unscored(g, os)
    }

    sixes ∀ test
  }

  //the initial partition of an unlabeled (unscored) graph
  //must be sorted by degree
  def iniPartitionUnscored(g: Graph, m: IsoM) =
    (ids(g) sortBy (i ⇒ (g degree i, i))) ≟ m.p.toList

  //checks that each vertex of degree zero sits in
  //its own cell
  def iniCellsDegree0Unscored(g: Graph, m: IsoM) =
    idsDeg0(g) ∀ { gi ⇒ m cellAtGraphIndex gi match {
      case Cell(start, size) ⇒ (size ≟ 1) && (m.p(start) ≟ gi)
    }}

  //checks that vertices of degree > 0 are grouped in a cell
  def iniCellsNon0Unscored(g: Graph, m: IsoM) =
    (idsDegNon0(g) groupBy g.degree toList) ∀ { case (d, is) ⇒ 
      is ∀ { gi ⇒ m.cellAtGraphIndex(gi).size ≟ is.size }
    }

  //checks that pi contains the start values
  //of all cells
  def iniPiUnscored(g: Graph, c: CellSets, m: IsoM) =
    BitSet(m.cellList map { _.start }: _*) == c.pi

  //checks that nonS contains the start values
  //of all non-singleton cells
  def iniNonSUnscored(g: Graph, c: CellSets, m: IsoM) = {
    val cs = m.cellList collect {
      case Cell(start, size) if size > 1 ⇒ start
    }

    BitSet(cs: _*) == c.nonS
  }

  //checks that alpha contains the start values
  //of all cells that do not hold vertices of
  //degree zero
  def iniAlphaUnscored(g: Graph, c: CellSets, m: IsoM) = {
    val cs = idsDegNon0(g) map { m.cellAtGraphIndex(_).start }

    BitSet(cs: _*) == c.alpha
  }

  //checks that all vertices of degree 0 are in the
  //same orbit
  def iniOrbitsDeg0Unscored(g: Graph, os: Orbits) = {
    val bs = BitSet(idsDeg0(g): _*)

    bs.toList ∀ (os(_) == bs)
  }

  //checks that all vertices of degree > 0 are in
  //their own singleton orbit
  def iniOrbitsDegNon0Unscored(g: Graph, os: Orbits) =
    idsDegNon0(g) ∀ (i ⇒ os(i) == BitSet(i))

  property("solve_isCanonical") = {
    val order = 6
    val gs = test.samples allOfOrder order
    val ps = Permutation ofSize order toList

    testCanonical(gs, ps)
  }

  //Groups graphs by their canonical form
  //Iterates then through the pairs, generates all permutations
  //of the canonical form and checks that it contains exactly the same
  //graphs as the list paired with the canonical form
  def testCanonical(gs: List[Graph], ps: List[Permutation]): Boolean = {
    implicit val LOrder = Order[Set[Edge]].toScalaOrdering

    def canonicalSet(g: Graph) = iso2.canonize(g).edges

    def transformCanonical(es: Set[Edge])(p: Permutation) =
      es map p.mapEdge

    def testPair(p: (Set[Edge], List[Graph])) = p match {
      case (es, gs) ⇒ {
        val sets = (ps map transformCanonical(es)).toSet
        val graphSets = gs map (_.edges) toSet

        sets ≟ graphSets
      }
    }

    val map = gs groupBy canonicalSet

    map.toList ∀ testPair
  }

  def refinePermutation(g: Graph): List[Int] = refine(g)._1

  def refineCells(g: Graph): List[Cell] = refine(g)._2

  def refine(g: Graph): (List[Int], List[Cell]) = iso2 refine g

  def ids(g: Graph) = List.range(0, g.order)

  def idsDeg0(g: Graph) = ids(g) filter { g.degree(_) ≟ 0 }

  def idsDegNon0(g: Graph) = ids(g) filter { g.degree(_) > 0 }

  def iniUnscored(g: Graph) = IsoI(g) initial { _ ⇒ 0 }
}

// vim: set ts=2 sw=2 et:
