package graph

import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import spire.syntax.cfor

//Less mutability, more recursion. Immutable BitSets are FAST
package object iso2 {
  type Cells = Array[Cell]
  type Degrees = Array[List[Int]]
  type Orbits = Array[BitSet]
  type Partition = Array[Int]

  /** Calculates the degree of a node i into a cell w */
  def degreeIn(i: Int, w: Cell)(implicit I: IsoI, M: IsoM): Int = {
    var res = 0
    I.g neighbors i foreach { i ⇒ if (M.c(i).start == w.start) res += 1 }
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

  def initialCellSets(g: Graph) = CellSets(
    BitSet(0), if (g.order > 1) BitSet(0) else BitSet(), BitSet(0)
  )

  def initialIsoM(g: Graph) = new IsoM(
    Array.range(0, g.order),
    Array.fill(g.order)(Cell(0, g.order))
  )

  //For testing
  def refine(g: Graph): (List[Int], List[Cell]) = {
    implicit val i = new IsoI(g)
    implicit val m = initialIsoM(g)
    refineSets(initialCellSets(g))

    (m.p.toList, m.c.toList)
  }

  def refineSets(sets: CellSets)
                (implicit I: IsoI, M: IsoM): CellSets = {
    def findShatterer(s: CellSets) = M fromCellIndex s.alpha.firstKey
    @tailrec
    def run(s: CellSets): CellSets =
      if (s.alpha.isEmpty || s.nonS.isEmpty) s
      else {
        val w = findShatterer(s)
        var next = CellSets(s.pi, s.nonS, s.alpha - w.start)
        s.nonS foreach { i ⇒ next = shatter(M fromCellIndex i, w, next) }

        run(next)
      }

      run(sets)
  }

  def searchTree(sets: CellSets)(implicit I: IsoI, M: IsoM): IsoResult = ???

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

      for {is ← ds; if is.nonEmpty} {
        val size = is.size
        val newCell = Cell(cursor, size)

        if (size > largestSize) { largestSize = size; largest = cursor }
        if (size > 1) { nonS += cursor }

        pi += cursor
        a += cursor

        is foreach { i ⇒ 
          M.p(cursor) = i
          M.c(i) = newCell
          cursor += 1
        }
      }

      if (!keepAll) { a -= largest }

      CellSets(pi, nonS, a)
    }
  }

  final case class Cell(start: Int, size: Int) {
    val end = start + size - 1
  }

  /** The immutable part of a graph isomorphism calculation */
  final class IsoI(val g: Graph) {
    private val ds: Degrees = Array.fill(g.degrees.max + 1)(Nil)

    def newDegrees: Degrees = ds.clone()
  }

  /** The (for efficiency reasons) mutable part of a graph isomorphism
    * calculation */
  final class IsoM(val p: Partition, val c: Cells) {
    def copy(): IsoM = new IsoM(p.clone(), c.clone())

    def fromCellIndex(i: Int): Cell = c(p(i))
  }

  final case class CellSets(
    pi: BitSet, //start indices of all cells
    nonS: BitSet, //start indices of non-singleton cells
    alpha: BitSet //start indices of possible shatterers
  )

  final class IsoResult(val orbits: Orbits, val p: Partition)(implicit I: IsoI) {
     
  }
}

// vim: set ts=2 sw=2 et:
