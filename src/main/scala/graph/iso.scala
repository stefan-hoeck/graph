package graph

import scala.collection.immutable.BitSet
import scala.collection.mutable.{BitSet ⇒ MBitSet}
import spire.syntax.cfor
import scalaz.syntax.order._
import scalaz.std.set._

/** Algorithms dealing with graph isomorphism detection and
  * canonical labelings
  */
object iso {

  //For testing
  def refine(g: Graph): (List[Int], List[Cell]) = {
    val i = new Iso(g)
    i.refine()

    (i.p.toList, i.c.toList)
  }
}

final case class Cell(start: Int, size: Int) {
  val end = start + size - 1
}

//cfor faster than Range.foreach
private[graph] final class Iso(g: Graph) {
  //the actual permutation of the original graph g
  //the value at p(i) is the vertex in g that will be
  //moved to vertex i in the new graph
  val p: Array[Int] = Array.range(0, g.order)

  //Mapping from vertex to cell
  val c: Array[Cell] = Array.fill(g.order)(Cell(0, g.order))

  //Size of Array for listing degrees
  //For molecules, this is typically small (5 or lower)
  val dsSize = g.degrees.max + 1

  //Start index in p of each cell
  var cells = BitSet(0)

  var nonSingletons = if (g.order > 1) BitSet(0) else BitSet()

  def fromCellIndex(i: Int): Cell = c(p(i))

  def canonical(): Array[Int] = {
    var result = p.clone
    var best: Set[Edge] = g.edges

    refine()

    def undoChanges {}
    def transmuteEdges: Set[Edge] = ???

    def searchTree() {
      if (nonSingletons.isEmpty) {
        val es = transmuteEdges
        if (es lt best) {
          result = p.clone
          best = es
        }
      }
      else {}
    }

    searchTree()

    result
  }

  def refine(alpha: BitSet = BitSet(0)) {
    var a = alpha

    def findShatterer: Cell = fromCellIndex(a.firstKey)

    //x: cell to be shattered, w: shattering cell
    def shatter(x: Cell, w: Cell) {
      val ds: Array[List[Int]] = Array.fill(dsSize)(Nil)
      var cursor = x.start
      var minD = Int.MaxValue
      var maxD = Int.MinValue

      //fill degrees (in reverse order to keep order of vertices
      //with same degree)
      //keep track of maximum and minimum degree found
      cfor(x.end)(_ >= x.start, _ - 1) { i ⇒ 
        val index = p(i)
        val d = degreeIn(index, w)

        minD = d min minD
        maxD = d max maxD
        ds(d) ::= index
      }

      if (minD != maxD) {
        var largestSize = -1 //size of the largest cell generated
        var largest = -1 //start index of largest cell
        val keepAll = a(x.start) //keep all fragments if x is in alpha

        nonSingletons -= cursor

        for {is ← ds; if is.nonEmpty} {
          val size = is.size
          val newCell = Cell(cursor, size)

          if (size > largestSize) { largestSize = size; largest = cursor }
          if (size > 1) { nonSingletons += cursor }

          cells += cursor
          a += cursor

          is foreach { i ⇒ 
            p(cursor) = i
            c(i) = newCell
            cursor += 1
          }
        }

        if (!keepAll) { a -= largest }
      }
    }

    while(a.nonEmpty && nonSingletons.nonEmpty) {
      val w = findShatterer
      a -= w.start

      nonSingletons foreach { i ⇒ shatter(fromCellIndex(i), w) }
    }
  }


  //about 15% faster than g neighbors count { c(_).start == w.start }
  def degreeIn(i: Int, w: Cell): Int = {
    var res = 0
    g neighbors i foreach { i ⇒ 
      if (c(i).start == w.start) res += 1
    }
    res
  }
}

// vim: set ts=2 sw=2 et:
