package graph

import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import spire.syntax.cfor
import scalaz._, Scalaz._

package object iso2 {
  private val EmptyBS = BitSet()

  private val ZeroBS = BitSet(0)

  /** Type alias for a mapping from a graph's vertex to a `Cell`
    *
    * The indexing is with respect of a vertex's index in the graph
    * that is to be canonized, not with respect to the actual
    * `Partition` on the way to a canonical form
    */
  type Cells = Array[Cell]

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
  type Orbits = Array[BitSet]

  /** An ordered partition of a graph's vertices used
    * by the algorithms in this module.
    *
    * A partition is typically wrapped in a `IsoM`
    * object which comes together with a `Cells`
    * array that tells us in what sets the elements
    * of the partition are grouped.
    *
    * As an example, consider Isobutane (main chain with
    * labels 0 to 3, side chain with label 4 connected
    * to vertice 1) whose indices
    * are in a first step grouped by degree. The resulting
    * partition will then be `(0, 3, 4, 2, 1), where vertices
    * 0, 3, and 4 are of degree 1, vertice 2 is of degree 2,
    * and vertice 1 is of degree 3.
    *
    * The corresponding `Cells` array will look like so:
    * (Cell(0, 3), Cell(4, 1), Cell(3, 1), Cell(0, 3), Cell(0, 3)).
    * Note that a cell gives the start index in the partition and
    * the number of elements in the cell. Note also that the
    * entries in the cell array are place at the index of a
    * vertice in the original graph. For instance, the tertiary
    * carbon has index 1 in the original graph. The cell array
    * at index 1 points to the cell with size 1 starting at position 4
    * in the actual partition.
    */
  type Partition = Array[Int]

  type Degrees = Array[List[Int]]

  def solve(g: Graph): (Permutation, Orbits) = IsoI(g).solve

  /** Calculates the degree of a vertice v into a cell w */
  def degreeIn(v: Int, w: Cell)(implicit I: IsoI, M: IsoM): Int = {
    var res = 0

    I.g neighbors v foreach { graphIndex ⇒ 
      if (M.cellAtGraphIndex(graphIndex).start == w.start) res += 1
    }

    res //benchmarked and optimized
  }

  /** Returns an array of lists containing at index i all vertices
    * in cell x that have a degree of i in cell w
    */
  def fillDegrees(x: Cell, w: Cell)(implicit I: IsoI, M: IsoM): Degrees = {
    val ds = I.newDegrees

    @tailrec def run(i: Int, minD: Int, maxD: Int): Degrees =
      if (i < x.start) { if (minD != maxD) ds else null }
      else {
        val index = M.p(i)
        val d = degreeIn(index, w)
        ds(d) ::= index

        run(i - 1, d min minD, d max maxD) //fill in reverse order
      }

    run(x.end, Int.MaxValue, Int.MinValue)
  }



  def mergeOrbits(a: Orbits, b: Orbits): Orbits = {
    val res = Array.tabulate(a.size){ i ⇒ a(i) | b(i) }
    val handled = Array.fill(a.size)(false)

    cfor(0)(_ < a.size, _ + 1){ i ⇒ 
      if (! handled(i) && b(i).size > 1) {
        handled(i) = true
        var newOrbit: BitSet = res(i)
        var mustVisit = newOrbit filterNot handled

        while (mustVisit.nonEmpty) {
          val head = mustVisit.head
          handled(head) = true

          mustVisit = mustVisit.tail 
          res(head) foreach { j ⇒ 
            if (! handled(j)) mustVisit += j
          }
          newOrbit += head
        }

        newOrbit foreach { j ⇒ res(j) = newOrbit }
      }
    }

    res
  }

  /** Generates the orbit sets from a permutation */
  def permutationToOrbits(p: Permutation, size: Int): Orbits = {
    val res = Array.fill(size)(BitSet.empty)

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

    res
  }

  //For testing
  def refine(g: Graph): (List[Int], List[Cell]) = {
    implicit val i = IsoI(g)
    implicit val m = i.initialIsoM

    refineSets(i.initialCellSets)

    m.lists
  }

  def refineSets(sets: CellSets)
                (implicit I: IsoI, M: IsoM): CellSets = {
    @tailrec
    def run(s: CellSets): CellSets =
      if (s.alpha.isEmpty || s.nonS.isEmpty) s
      else {
        val w = M findShatterer s
        var next = CellSets(s.pi, s.nonS, s.alpha - w.start)

        s.nonS foreach { partitionIndex ⇒ 
          next = shatter(M cellAtPartitionIndex partitionIndex, w, next)
        }

        run(next)
      }

    run(sets)
  }

  def searchTree(sets: CellSets, best: IsoResult, m: IsoM)
    (implicit I: IsoI): (IsoResult, Boolean) = {
    val refined = refineSets(sets)(I, m)

    if (refined.nonS.isEmpty) {
      best next m.permutation
    }
    else {
      val cell = m cellAtPartitionIndex refined.nonS.head
      var handled: BitSet = EmptyBS
      var ignore: BitSet = EmptyBS
      var result = best
      var orbitsChanged = false

      cfor(cell.start)(_ <= cell.end, _ + 1){ partitionIndex ⇒ 
        val graphIndex = m p partitionIndex
        if (! ignore(graphIndex)) {
          val p = splitAt(refined, m, partitionIndex)
          val (nextM, nextSets) = p
          handled += graphIndex
          ignore ++= result.orbitsOriginal(graphIndex)
          val (newRes, changed) = searchTree(nextSets, result, nextM)
          if (changed) { orbitsChanged = true }
          result = newRes
        }
      }

      (result, orbitsChanged)
    }
  }

  def splitAt(c: CellSets, m: IsoM, partitionIndex: Int)
    : (IsoM, CellSets) = {
    val cell = m cellAtPartitionIndex partitionIndex
    val newCell = Cell(cell.start, cell.size - 1)
    val newIsoM = m.copy()
    val image = m p partitionIndex
    val newNonS = if (newCell.size > 1) c.nonS else c.nonS - cell.start

    cfor(cell.start)(_ < cell.end, _ + 1){ pi ⇒ 
      if (pi >= partitionIndex) newIsoM.p(pi) = newIsoM.p(pi + 1)
      newIsoM.setCellAtPartitionIndex(pi, newCell)
    }

    newIsoM.p(cell.end) = image
    newIsoM.setCellAtPartitionIndex(cell.end, Cell(cell.end, 1))

    val sets = CellSets(c.pi + cell.end, newNonS, BitSet(cell.end))

    (newIsoM, sets)
  }

  def shatter(x: Cell, w: Cell, sets: CellSets)
             (implicit I: IsoI, M: IsoM): CellSets = fillDegrees(x, w) match {
    case null ⇒ sets
    case ds   ⇒ {
      var cursor = x.start
      var CellSets(pi, nonS, a) = sets
      var largestSize = -1 //size of the largest cell generated
      var largest = -1 //start index of largest cell
      val keepAll = a(x.start) //keep all fragments if x is in alpha

      nonS -= cursor

      for {graphIndices ← ds; if graphIndices.nonEmpty} {
        val size = graphIndices.size
        val newCell = Cell(cursor, size)

        if (size > largestSize) { largestSize = size; largest = cursor }
        if (size > 1) { nonS += cursor }

        pi += cursor
        a += cursor

        graphIndices foreach { i ⇒ 
          M.p(cursor) = i
          M.setCellAtGraphIndex(i, newCell)
          cursor += 1
        }
      }

      if (!keepAll && largest >= 0) { a -= largest }

      CellSets(pi, nonS, a)
    }
  }

  /** A cell in an ordered partition of a graph's nodes
    *
    * `start` represents the initial index in the
    * partition array, `size` the number of consecutive
    * entries in the same array that contain the elements
    * of this cell
    */
  final case class Cell(start: Int, size: Int) {
    /** The index of the last item of this cell in the partition array */
    val end = start + size - 1
  }

  /** The immutable part of a graph isomorphism calculation */
  final case class IsoI(val g: Graph) {
    private val ds: Degrees = Array.fill(g.degrees.max + 1)(Nil)

    val edges: List[Edge] = g.edges.toList

    def order: Int = g.order

    /** Returns a new array of empty lists that can be filled
      * when generating the degrees the elements of one cell
      * into another cell.
      */
    def newDegrees: Degrees = ds.clone()

    def initialCellSets = 
      CellSets(ZeroBS, if (g.order > 1) ZeroBS else EmptyBS, ZeroBS)

    def initialIsoM = new IsoM(
      Array.range(0, order),
      Array.fill(order)(Cell(0, order))
    )

    def initialOrbits: Orbits = Array.tabulate(order)(BitSet(_))

    def initialResult: IsoResult = IsoResult(initialOrbits,
      ∅[Permutation], List.fill(order)(Edge(order, order - 1)))(this)

    def solve: (Permutation, Orbits) = {
      val res =
        searchTree(initialCellSets, initialResult, initialIsoM)(this)._1

      (res.p, res.orbitsOriginal)
    }
  }

  /** The (for efficiency reasons) mutable part of a graph isomorphism
    * calculation
    */
  final class IsoM(val p: Partition, c: Cells) {
    /** Creates a clone of the mutable parts of this object */
    def copy(): IsoM = new IsoM(p.clone(), c.clone())

    def cellAtGraphIndex(i: Int): Cell = c(i)

    def setCellAtGraphIndex(i: Int, cell: Cell) { c(i) = cell }

    def cellAtPartitionIndex(i: Int): Cell = c(p(i))

    def setCellAtPartitionIndex(i: Int, cell: Cell) { c(p(i)) = cell }

    def findShatterer(s: CellSets): Cell =
      cellAtPartitionIndex(s.alpha.firstKey)

    def permutation: Permutation = Permutation(p)

    private[graph] def lists: (List[Int], List[Cell]) =
      (p.toList, c.toList)

    def cellList: List[Cell] = c.distinct sortBy { _.start } toList

    def partitionList: List[List[Int]] =
      cellList map { c ⇒ c.start to c.end map p toList }

    override def toString = {
      def inner = partitionList map { _ mkString ", " } mkString " | "

      s"{$inner}"
    }
  }

  final case class CellSets(
    pi: BitSet, //start indices of all cells
    nonS: BitSet, //start indices of non-singleton cells
    alpha: BitSet //start indices of possible shatterers
  )

  /** Result of solving the graph isomorphism problem.
    *
    * `orbitsOriginal` contains the orbits of the graph's automorphism group
    * (given in the indices of the original graph)
    * (see also description of `Orbits` type alias)
    *
    * `p` is the permutation that transforms the given graph to
    * its canonically labeled form
    *
    * `edges` is a sorted image of the graph's edge set under the 
    * given permutation. This is used to compare different `IsoResults`,
    * both for finding the best one (the one giving the smallest
    * sorted list of edges) and for finding automorphisms (if two
    * distinct permutations lead to the same canonical form)
    */
  final case class IsoResult(
      orbitsOriginal: Orbits,
      p: Permutation,
      edges: List[Edge])(implicit I: IsoI) {
      
    def next(p1: Permutation): (IsoResult, Boolean) = { 
      val inv = p1.inverse
      val es = I.edges map inv.mapEdge sorted

      edges ?|? es match {
        case Ordering.LT ⇒ (this, false)
        case Ordering.GT ⇒ (IsoResult(orbitsOriginal, inv, es), false)
        case Ordering.EQ ⇒ {
          val iso = p1 compose p
          val newOs =
            mergeOrbits(orbitsOriginal, permutationToOrbits(iso, I.order))

          (IsoResult(newOs, p, edges), true)
        }
      }
    }

  }
}

// vim: set ts=2 sw=2 et:
