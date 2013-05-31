package graph

import scala.collection.mutable.BitSet

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

private[graph] final class Iso(g: Graph) {
  //the actual permutation of the original graph g
  //the value at p(i) is the vertex in g that will be
  //moved to vertex i in the new graph
  val p: Array[Int] = Array.range(0, g.order)

  //Mapping from vertex to cell
  val c: Array[Cell] = Array.fill(g.order)(Cell(0, g.order))

  //The maximum degree of vertices in g
  //For molecules, this is typically limited to 4; on rare occasions
  //it can be higher.
  val maxDegree = g.degrees.max

  //Start index in p of each cell
  val cells = BitSet(0)

  def fromCellIndex(i: Int): Cell = c(p(i))

  def refine(alpha: BitSet = BitSet(0)) {

    def findShatterer: Cell = fromCellIndex(alpha.head)

    while(! alpha.isEmpty && cells.size < g.order) {
      val w = findShatterer
      alpha -= w.start

      cells foreach { i ⇒ 
        val x = fromCellIndex(i)
        if (x.size > 1) shatter(x, w, alpha)
      }
    }
  }

  //x: cell to be shattered, w: shattering cell
  def shatter(x: Cell, w: Cell, alpha: BitSet) {
    var cursor = x.start
    var minD = Int.MaxValue
    var maxD = Int.MinValue
    val ds = Array.fill(maxDegree + 1)(List[Int]())

    //fill degrees (in reverse order to keep order of vertices
    //with same degree)
    x.end.to(x.start, -1) foreach { i ⇒ 
      val index = p(i)
      val d = degreeIn(index, w)

      if (d < minD) minD = d
      if (d > maxD) maxD = d
      ds(d) ::= index
    }

    if (minD != maxD) {
      var largestSize = -1 //size of the largest cell generated
      var largest = -1 //start index of largest cell
      //if x is in alpha, keep all fragments, else add all but one of
      //the largest
      val keepAll = alpha(x.start)
      ds foreach { is ⇒ 
        if (is.nonEmpty) {
          val size = is.size
          val newCell = Cell(cursor, size)

          if (size > largestSize) { largestSize = size; largest = cursor }

          cells += cursor
          alpha += cursor

          is foreach { i ⇒ 
            p(cursor) = i
            c(i) = newCell
            cursor += 1
          }
        }
      }
      if (!keepAll) { alpha -= largest }
    }
  }

  def degreeIn(i: Int, w: Cell): Int =
    g neighbors i count (c(_).start == w.start)
}

// vim: set ts=2 sw=2 et:
