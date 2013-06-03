package graph

import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import spire.syntax.cfor
import scalaz._, Scalaz._

//Less mutability, more recursion. Immutable BitSets are FAST
package object iso2 {
  type Cells = Array[Cell]
  type Degrees = Array[List[Int]]
  type Orbits = Array[BitSet]
  type Partition = Array[Int]

  def solve(g: Graph): (Permutation, Orbits) = {
    val res = searchTree(initialCellSets(g),
      initialResult(g), initialIsoM(g))(new IsoI(g))._1

    (res.p, res.orbits)
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

  def initialCellSets(g: Graph) = CellSets(
    BitSet(0), if (g.order > 1) BitSet(0) else BitSet(), BitSet(0)
  )

  def initialIsoM(g: Graph) = new IsoM(
    Array.range(0, g.order),
    Array.fill(g.order)(Cell(0, g.order))
  )

  def initialOrbits(g: Graph): Orbits =
    Array.tabulate(g.order)(BitSet(_))

  def initialResult(g: Graph) = IsoResult(initialOrbits(g),
    ∅[Permutation], List.fill(g.order)(Edge(g.order, g.order - 1)))

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
        println(s"Shattering with $w")
        var next = CellSets(s.pi, s.nonS, s.alpha - w.start)
        s.nonS foreach { i ⇒ next = shatter(M fromCellIndex i, w, next) }

        assert(next != s, s"$next")
        run(next)
      }

    run(sets)
  }

  def searchTree(sets: CellSets, best: IsoResult, m: IsoM)
    (implicit I: IsoI): (IsoResult, Boolean) = {
    println(s"Refining: $m; $sets")
    val refined = refineSets(sets)(I, m)
    println(s"Refined to: $m")

    if (refined.nonS.isEmpty) {
      val perm = Permutation(m.p)
      println(s"Solution found: $perm")
      best next perm
    } else {
      val cell = m c refined.nonS.head
      var handled: BitSet = BitSet()
      var ignore: BitSet = BitSet()
      var result = best
      var orbitsChanged = false

      cfor(cell.start)(_ <= cell.end, _ + 1){ i ⇒ 
        if (! ignore(i)) {
          val p = splitAt(refined, m, i)
          val (nextM, nextSets) = p
          println(s"Splitting at: $i, $m")
          println(nextSets)
          handled += i
          ignore ++= result.orbits(i)
          val (newRes, changed) = searchTree(nextSets, result, nextM)
          if (changed) { orbitsChanged = true }
          result = newRes
        }
      }

      (result, orbitsChanged)
    }
  }

  def splitAt(c: CellSets, m: IsoM, i: Int): (IsoM, CellSets) = {
    val cell = m.c(m.p(i))
    println(s"Splitting $cell at $i")
    val newCell = Cell(cell.start, cell.size - 1)
    val newIsoM = m.copy()
    val image = m.p(i)
    val newNonS = if (newCell.size > 1) c.nonS else c.nonS - cell.start

    cfor(cell.start)(_ < cell.end, _ + 1){ j ⇒ 
      if (j >= i) newIsoM.p(j) = newIsoM.p(j + 1)
      newIsoM.c(j) = newCell
    }

    newIsoM.p(cell.end) = image
    newIsoM.c(cell.end) = newCell
    newIsoM.c(image) = Cell(cell.end, 1)

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

      if (!keepAll && largest >= 0) { a -= largest }

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

    override def toString = s"${p.toList}; ${c.toList}"
  }

  final case class CellSets(
    pi: BitSet, //start indices of all cells
    nonS: BitSet, //start indices of non-singleton cells
    alpha: BitSet //start indices of possible shatterers
  )

  final case class IsoResult(
      orbits: Orbits,
      p: Permutation,
      edges: List[Edge]) {
    def next(p1: Permutation)(implicit I: IsoI): (IsoResult, Boolean) = { 
      val inv = p1.inverse
      val es = I.g.edges.toList map inv.mapEdge sorted

      println(es)
      println(edges)
      println(edges ?|? es)

      edges ?|? es match {
        case Ordering.LT ⇒ (this, false)
        case Ordering.GT ⇒ (IsoResult(orbits, inv, es), false)
        case Ordering.EQ ⇒ {
          val iso = p compose inv.inverse
          val newOs = mergeOrbits(orbits, permutationToOrbits(inv, I.g.order))
          println(s"Automorphism found: $iso")

          (IsoResult(newOs, p, edges), true)
        }
      }
    }
  }
}

// vim: set ts=2 sw=2 et:
