package graph

/** Algorithms dealing with graph isomorphism detection and
  * canonical labelings
  */
object iso {
  private[iso] type Permutation = Array[Int] //mapping from new id to old id
  private[iso] type CellTo = Array[Cell] //mapping from id to Cell
  private[iso] type Degrees = Array[List[Int]] //mapping from id to Cell

  private[iso] final case class Cell(start: Int, size: Int) {
    val end = start + size - 1
  }

  private[iso] final class Iso(g: Graph) {
    //the actual permutation of the original graph g
    //the value at p(i) is the vertex in g that will be
    //moved to vertex i in the new graph
    val p: Permutation = Array.range(0, g.order)

    //Mapping from vertex to cell
    val c: CellTo = Array.fill(g.order)(Cell(0, g.order - 1))

    //The maximum degree of vertices in g
    //For molecules, this is typically limited to 4; on rare occasions
    //it can be higher.
    val maxDegree = g.degrees.max

    val ds: Degrees = Array.fill(maxDegree + 1)(Nil)

    //x: cell to be shattered, w: shattering cell
    def shatter(x: Cell, w: Cell) {
      clearDegrees()

      //fill degrees (in reverse order to keep order of vertices
      //with same degree)
      x.end.to(x.start, -1) foreach { i ⇒ ds(degreeIn(i, w)) ::= p(i) }
      
      var actual = x.start

      ds foreach { is ⇒ 
        if (is.nonEmpty) {
          val newCell = Cell(actual, is.size)
          is foreach { i ⇒ 
            p(actual) = i
            c(actual) = newCell
            actual += 1
          }
        }
      }
    }

    def clearDegrees() { 0 until maxDegree foreach { i ⇒ ds(i) = Nil } }

    def degreeIn(i: Int, w: Cell): Int =
      g neighbors p(i) count (c(_).start == w.start)
  }
}

// vim: set ts=2 sw=2 et:
