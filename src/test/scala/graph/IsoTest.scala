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

  //the initial partition of an unlabeled (unscored) graph
  //must be sorted by degree
  property("initial_partition_unscored") = {
    def test(g: Graph) = 
      (ids(g) sortBy { i ⇒ (g degree i, i) }) ≟
      iniIsoM(g).p.toList

    sixes ∀ test
  }

  //checks that each vertex of degree zero sits in
  //its own cell
  property("initial_cells_degree0_unscored") = {
    def test(g: Graph) = {
      val m = iniIsoM(g)

      idsDeg0(g) ∀ { gi ⇒ m cellAtGraphIndex gi match {
        case Cell(start, size) ⇒ (size ≟ 1) && (m.p(start) ≟ gi)
      }}
    }

    sixes ∀ test
  }

  //checks that vertices of degree > 0 are grouped in a cell
  property("initial_cells_degree0_unscored") = {
    def test(g: Graph) = {
      val m = iniIsoM(g)

      (idsDegNon0(g) groupBy g.degree toList) ∀ { case (d, is) ⇒ 
        is ∀ { gi ⇒ m.cellAtGraphIndex(gi).size ≟ is.size }
      }
    }

    sixes ∀ test
  }

  //checks that pi contains the start values
  //of all cells
  property("initial_pi_unscored") = {
    def test(g: Graph) = {
      val (m, c, o) = iniUnscored(g)

      BitSet(m.cellList map { _.start }: _*) == c.pi
    }

    sixes ∀ test
  }

  //checks that nonS contains the start values
  //of all non-singleton cells
  property("initial_nonS_unscored") = {
    def test(g: Graph) = {
      val (m, c, o) = iniUnscored(g)
      val cs = m.cellList collect {
        case Cell(start, size) if size > 1 ⇒ start
      }

      BitSet(cs: _*) == c.nonS
    }

    sixes ∀ test
  }

  //checks that alpha contains the start values
  //of all cells that do not hold vertices of
  //degree zero
  property("initial_nonS_unscored") = {
    def test(g: Graph) = {
      val (m, c, o) = iniUnscored(g)
      val cs = idsDegNon0(g) map { m.cellAtGraphIndex(_).start }

      BitSet(cs: _*) == c.alpha
    }

    sixes ∀ test
  }

  //checks that all vertices of degree 0 are in the
  //same orbit
  property("initial_orbits_degree0_unscored") = {
    def test(g: Graph) = {
      val os = iniOrbits(g)
      val bs = BitSet(idsDeg0(g): _*)

      bs.toList ∀ { i ⇒ os(i) == bs }
    }

    sixes ∀ test
  }

  //checks that all vertices of degree > 0 are in
  //their own singleton orbit
  property("initial_orbits_degreeNon0_unscored") = {
    def test(g: Graph) = {
      val os = iniOrbits(g)
      idsDegNon0(g) ∀ { i ⇒ os(i) == BitSet(i) }
    }

    sixes ∀ test
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

  def ids(g: Graph) = List.range(0, g.order)

  def idsDeg0(g: Graph) = ids(g) filter { g.degree(_) ≟ 0 }

  def idsDegNon0(g: Graph) = ids(g) filter { g.degree(_) > 0 }

  def iniIsoM(g: Graph) = iniUnscored(g)._1

  def iniCells(g: Graph) = iniUnscored(g)._2

  def iniOrbits(g: Graph) = iniUnscored(g)._3

  def iniUnscored(g: Graph) = IsoI(g) initial { _ ⇒ 0 }
}

// vim: set ts=2 sw=2 et:
