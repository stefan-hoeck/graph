package graph

import scala.collection.mutable.BitSet

/** Algorithms dealing with graph isomorphism detection and
  * canonical labelings
  */
object iso {
  private[iso] type Permutation = Array[Int] //mapping from new id to old id
  private[iso] type CellTo = Array[Cell] //mapping from id to Cell

  //mapping from degree to vertices
  private[iso] type Degrees = Array[List[Int]]

  private[graph] final case class Cell(start: Int, size: Int) {
    val end = start + size - 1
  }

  private[graph] final class Iso(g: Graph) {
    //the actual permutation of the original graph g
    //the value at p(i) is the vertex in g that will be
    //moved to vertex i in the new graph
    val p: Permutation = Array.range(0, g.order)

    //Mapping from vertex to cell
    val c: CellTo = Array.fill(g.order)(Cell(0, g.order))

    //The maximum degree of vertices in g
    //For molecules, this is typically limited to 4; on rare occasions
    //it can be higher.
    val maxDegree = g.degrees.max

    val cells = BitSet(0)

    def refine(alpha: BitSet = BitSet(0)) {
      while(! alpha.isEmpty && cells.size < g.order) {
        val w = findShatterer(alpha)
        alpha -= w.start

        cells foreach { i ⇒ 
          val x = c(i)
          if (c.size > 1) shatter(x, w, alpha)
        }
      }
    }

    def findShatterer(alpha: BitSet): Cell = c(alpha.head)

    //x: cell to be shattered, w: shattering cell
    def shatter(x: Cell, w: Cell, alpha: BitSet) {
      val ds: Degrees = Array.fill(maxDegree + 1)(Nil)

      //fill degrees (in reverse order to keep order of vertices
      //with same degree)
      x.end.to(x.start, -1) foreach { i ⇒ ds(degreeIn(i, w)) ::= p(i) }
      
      var actual = x.start
      var largestSize = 0
      var largest = x.start
      val keepAll = alpha(x.start)

      ds foreach { is ⇒ 
        if (is.nonEmpty) {
          val size = is.size
          val newCell = Cell(actual, size)

          if (size > largestSize) { largestSize = size; largest = actual }
          cells += actual
          alpha += actual

          is foreach { i ⇒ 
            p(actual) = i
            c(actual) = newCell
            actual += 1
          }
        }
      }

      if (!keepAll) alpha -= largest
    }

    def degreeIn(i: Int, w: Cell): Int =
      g neighbors p(i) count (c(_).start == w.start)
  }
}

// vim: set ts=2 sw=2 et:
