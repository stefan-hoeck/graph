package graph

import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import spire.syntax.cfor

//Less mutability, more recursion. Immutable BitSets are FAST
package object iso2 {
  type Partition = Array[Int]
  type Cells = Array[Cell]
  type Degrees = Array[List[Int]]

  final case class Cell(start: Int, size: Int) {
    val end = start + size - 1
  }

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

  //For testing
  def refine(g: Graph): (List[Int], List[Cell]) = {
    implicit val i = new IsoI(g)
    implicit val m = initialIsoM(g)
    refineSets(initialShatterSets(g))

    (m.p.toList, m.c.toList)
  }

  def refineSets(sets: ShatterSets)
            (implicit I: IsoI, M: IsoM): ShatterSets = {
    def findShatterer(s: ShatterSets) = M fromCellIndex s.alpha.firstKey
    @tailrec
    def run(s: ShatterSets): ShatterSets =
      if (s.alpha.isEmpty || s.nonS.isEmpty) s
      else {
        val w = findShatterer(s)
        var next = ShatterSets(s.pi, s.nonS, s.alpha - w.start)
        s.nonS foreach { i ⇒ next = shatter(M fromCellIndex i, w, next) }

        run(next)
      }

      run(sets)
  }

  def shatter(x: Cell, w: Cell, sets: ShatterSets)
             (implicit I: IsoI, M: IsoM): ShatterSets = fillDegrees(x, w) match {
    case null ⇒ sets
    case ds   ⇒ {
      var cursor = x.start
      var ShatterSets(pi, nonS, a) = sets
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

      ShatterSets(pi, nonS, a)
    }
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

  def initialShatterSets(g: Graph) = ShatterSets(
    BitSet(0), if (g.order > 1) BitSet(0) else BitSet(), BitSet(0)
  )

  def initialIsoM(g: Graph) = new IsoM(
    Array.range(0, g.order),
    Array.fill(g.order)(Cell(0, g.order))
  )

  final case class ShatterSets(
    pi: BitSet, //start indices of all cells
    nonS: BitSet, //start indices of non-singleton cells
    alpha: BitSet //start indices of possible shatterers
  )
}

// vim: set ts=2 sw=2 et:
