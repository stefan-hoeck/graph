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

  def canonize(g: Graph): Graph = solve(g) match {
    case (p, _) ⇒ p mapGraph g
  }

  def solve(g: Graph): (Permutation, Orbits) = IsoI(g) solve { _ ⇒ 0 }

  /** Returns an array of lists containing at index i all vertices
    * in cell x that have a degree of i in cell w
    */
  private def fillDegrees(x: Cell, w: Cell)
                         (implicit I: IsoI, M: IsoM): Degrees = {
    val ds = I.newDegrees

    def degreeInW(v: Int): Int =
      I.g neighbors v count (M.cellAtGraphIndex(_).start == w.start)

    @tailrec def run(i: Int, minD: Int, maxD: Int): Degrees =
      if (i < x.start) { if (minD != maxD) ds else null }
      else {
        val index = M.p(i)
        val d = degreeInW(index)
        ds(d) ::= index

        run(i - 1, d min minD, d max maxD) //fill in reverse order
      }

    run(x.end, Int.MaxValue, Int.MinValue)
  }

  //For testing
  def refine(g: Graph): (List[Int], List[Cell]) = {
    implicit val i = IsoI(g)
    implicit val m = i.initialIsoM

    refineSets(i.initialCellSets)

    m.lists
  }

  private def refineSets(sets: CellSets)
                        (implicit I: IsoI, M: IsoM): CellSets = {
    @tailrec def run(s: CellSets): CellSets =
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

      cell foreach { partitionIndex ⇒ 
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

    def foreach(f: Int ⇒ Unit) { cfor(start)(_ <= end, 1+)(f) }
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

    def initialResult(o: Orbits): IsoResult = IsoResult(
      o, ∅[Permutation], List.fill(order)(Edge(order, order - 1)))(this)

    /** Solves the graph isomorphism problem using an additional
      * scoring function. This can be used to solve the problem
      * for labeled graphs.
      *
      * Vertices are first sorted by degree and then by their
      * scoring function. Vertices of degree 0 are then
      * scored by their index since identical vertices of degree 0
      * are in the same orbit anyway.
      */
    def solve(score: Int ⇒ Int): (Permutation, Orbits) =
      initial(score) match {
        case (m, sets, o) ⇒ {
          val res = searchTree(sets, initialResult(o), m)(this)._1

          (res.p, res.orbitsOriginal)
        }
      }

    def initial(score: Int ⇒ Int): (IsoM, CellSets, Orbits) = {
      type Triple = (Int, Int, Int) //(degree, score, index)
      def triple(gi: Int): Triple = (g degree gi, score(gi), gi)
      val m = initialIsoM
      val orbits = Array.tabulate(g.order)(BitSet(_)) //initial orbits
      var pi = EmptyBS //start indices of initial cells
      var nonS = EmptyBS //start indices non-singleton cells
      var alpha = EmptyBS //start indices of shattering cells
      var zeroOrbit = EmptyBS //accumulates vertices of degree zero
      var highest = (-1, 0) //score of highest vertex visited so far
      var cursor = 0 //actual position in the array
      var cellStart = 0 //start index actual cell
      val (nulls, rest) =
        (0 until g.order map triple sorted) partition (_._1 == 0)

      def adjustZeroOrbits() {
        zeroOrbit foreach { gi2 ⇒ orbits(gi2) = zeroOrbit }
      }

      //vertices of degree zero
      // -belong to the same orbit if they have the same score
      // -all are added to their own trivial cell which is added to pi
      //  but not to alpha
      nulls foreach { case (d, s, gi) ⇒ 
        m.p(cursor) = gi
        m.setCellAtGraphIndex(gi, Cell(cursor, 1))
        pi += cursor
        cursor += 1
        cellStart = cursor

        //check if found a set of zero degree vertices with new score
        if (highest < (d, s)) {
          //yes ⇒ update orbits then reset zeroOrbit
          adjustZeroOrbits()
          zeroOrbit = BitSet(gi)
          highest = (0, s)
        } else {
          //no ⇒ add vertex to actual zeroOrbit set
          zeroOrbit += gi
        }
      }

      adjustZeroOrbits()

      def cellClose() {
        val newCell = Cell(cellStart, cursor - cellStart)
        cellStart = cursor

        newCell foreach { m.setCellAtPartitionIndex(_, newCell) }

        if (newCell.size > 1) nonS += newCell.start
      }

      rest foreach { case (d, s, gi) ⇒ 
        m.p(cursor) = gi
        
        //start new cell
        if (highest < (d, s)) {
          pi += cursor
          alpha += cursor
          cellClose()
          highest = (d, s)
        }

        cursor += 1
      }

      cellClose() //closes the last opened cell

      (m, CellSets(pi, nonS, alpha), Orbits(orbits))
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

    //For testing
    private[graph] def lists: (List[Int], List[Cell]) =
      (p.toList, c.toList)

    def cellList: List[Cell] = c.distinct sortBy (_.start) toList

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
    * (see also description of `Orbits` class)
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
          val newOs = orbitsOriginal merge iso.orbits

          (IsoResult(newOs, p, edges), true)
        }
      }
    }
  }
}

// vim: set ts=2 sw=2 et:
