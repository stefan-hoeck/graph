package graph.benchmark

import collection.immutable.{IntMap, TreeMap, SortedMap}
import com.google.caliper.Param

/** Results:
  *
  * Sizes: 10 = small, 100 = medium, 1000 = large
  * Number of edges: n * (n - 1) / 2:
  * n = 10: 45
  * n = 100: 4950
  * n = 1000: 499500
  * n = 10 represents typical organic molecules with implicit H-Atoms
  * quite well
  *
  * CleverEdge creation is about 1/3 slower than BasicEdge creation
  * Clever map creation using an IntMap is about 1/3 faster for small
  * and medium sized maps, and about twice as fast for
  * large maps.
  *
  * CleverEdge lookup is slightly slower for small maps, about
  * 1/4 slower for medium maps, and about twice as fast for
  * large maps.
  */
class EdgeBenchmark extends BMark {
  @Param(Array("10", "100", "1000")) val size = 0

  private var bMap: Map[BasicEdge,BasicEdge] = Map()
  private var cMap: Map[Int,CleverEdge] = Map()
  private var tMap: Map[BasicEdge,BasicEdge] = Map()
  private var sMap: Map[BasicEdge,BasicEdge] = Map()

  override def setUp() {
    //initialize caches
    val b = BasicEdge(0, 1)
    val c = CleverEdge(0, 1)

    bMap = basicMap
    cMap = cleverMap
    tMap = treeMap
    sMap = sortedMap
  }

  def timeBasicCreation(n: Int) = run(n){ basicEdges }

  def timeCleverCreation(n: Int) = run(n){ cleverEdges }

  def timeBasicMapCreation(n: Int) = run(n){ basicMap }

  def timeCleverMapCreation(n: Int) = run(n){ cleverMap }

  def timeTreeMapCreation(n: Int) = run(n){ treeMap }

  def timeSortedMapCreation(n: Int) = run(n){ sortedMap }

  def timeTreeMapLookup(n: Int) = run(n){ basicEdges map tMap.get }

  def timeSortedMapLookup(n: Int) = run(n){ basicEdges map sMap.get }

  def timeBasicLookup(n: Int) = run(n){ basicEdges map bMap.get }

  def timeCleverLookup(n: Int) = run(n){
    cleverEdges map { e ⇒ cMap get e.hashCode }
  }

  private def basicEdges = 
    for { i ← 1 to size; j ← 0 until i } yield BasicEdge(i, j)

  private def cleverEdges = 
    for { i ← 1 to size; j ← 0 until i } yield CleverEdge(i, j)
    

  private def basicMap = Map(basicEdges map { e ⇒ e → e }: _*)

  private def treeMap = TreeMap(basicEdges map { e ⇒ e → e }: _*)

  private def sortedMap = SortedMap(basicEdges map { e ⇒ e → e }: _*)

  private def cleverMap =
    IntMap(cleverEdges map { e ⇒ e.hashCode → e }: _*)
}

final class BasicEdge(val a: Int, val b: Int) {
  override val hashCode = a + b * 7919

  override def equals(that: Any) = that match {
    case e: BasicEdge ⇒ e.a == a && e.b == b
    case _       ⇒ false
  }
}

object BasicEdge {
  def apply(a: Int, b: Int): BasicEdge = {
    require (a >= 0 && b >= 0)

    if (a < 250 && b < 250) cache(a)(b)
    else create(a, b)
  }

  private def create(a: Int, b: Int): BasicEdge = {
    if (a < b) new BasicEdge(a, b) else new BasicEdge (b, a)
  }

  private[this] val cache = Array.tabulate(250, 250)(create)

  implicit val Ord: Ordering[BasicEdge] = new Ordering[BasicEdge] {
    def compare(a: BasicEdge, b: BasicEdge) =
      if (a.b == b.b) a.a compare b.a
      else a.b compare b.b
  }
}

final class CleverEdge(val a: Int, val b: Int) {
  override val hashCode = (b * (b - 1)) / 2 + a

  override def equals(that: Any) = that match {
    case e: CleverEdge ⇒ e.a == a && e.b == b
    case _       ⇒ false
  }
}

object CleverEdge {
  private def valid(i: Int) = i >= 0 && i <= 100000

  def apply(a: Int, b: Int): CleverEdge = {
    require(valid(a) && valid(b) && a != b)

    if (a < 250 && b < 250) cache(a)(b)
    else create(a, b)
  }

  private def create(a: Int, b: Int): CleverEdge = {
    if (a < b) new CleverEdge(a, b) else new CleverEdge(b, a)
  }

  private[this] val cache = Array.tabulate(250, 250)(create)
}

// vim: set ts=2 sw=2 et:
