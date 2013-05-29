package graph.benchmark

import graph.{LGraph, Edge}
import collection.mutable.PriorityQueue
import com.google.caliper.Param
import java.util.{PriorityQueue ⇒ JQueue}
import scalaz.Heap


/** Results: 
  [info] size benchmark       us linear runtime
  [info]   10      Heap     31.3 =
  [info]   10     Queue     16.7 =
  [info]   10    JQueue     13.6 =
  [info]  100      Heap   3058.5 =
  [info]  100     Queue   1835.1 =
  [info]  100    JQueue   1337.8 =
  [info] 1000      Heap 353595.4 ==============================
  [info] 1000     Queue 180303.2 ===============
  [info] 1000    JQueue 149404.0 ============

  JQueue is about 25% faster than Scala Queue and more than
  twice as fast than scalaz.Heap
  */
class Dijkstra extends BMark {
  import Dijkstra._

  @Param(Array("10", "100", "1000")) val size = 0

  var edges: Map[Edge,Edge] = Map.empty
  var graph: LGraph[Edge,Int] = LGraph.empty

  def weight(e: Edge): Long = (e.b * (e.b - 1)) / 2 + e.a

  override def setUp() {
    edges = (1 to size) map { i ⇒ val e = Edge(i, i - 1); e → e } toMap

    graph = LGraph[Edge,Int](0 to size toVector, edges)
  }
  
  def timeHeap(n: Int) = run(n){
    var a: Any = 0

    0 until size foreach { i ⇒ a = dijkstra(graph)(i, weight) }
  }
  
  def timeQueue(n: Int) = run(n){
    var a: Any = 0

    0 until size foreach { i ⇒ a = dijkstraQ(graph)(i, weight) }
  }
  
  def timeJQueue(n: Int) = run(n){
    var a: Any = 0

    0 until size foreach { i ⇒ a = dijkstraJQ(graph)(i, weight) }
  }
}

object Dijkstra {

  def dijkstra[E,V](g: LGraph[E,V])(start: Int, weight: E ⇒ Long)
    : (Array[Long],Array[Int]) = {

    val min = Array.fill(g.order)(Long.MaxValue)
    val visited = Array.fill(g.order)(false)
    val p = Array.fill(g.order)(-1)
    var h = Heap singleton (0L, start)
    min(start) = 0L

    while (! h.isEmpty) {
      val ((l, i), rest) = h.uncons.get
      h = rest
      if (!visited(i)) {
        visited(i) = true
        g neighbors i foreach { n ⇒ 
          val w = l + weight(g eLabel Edge(i, n))
          if (w < min(n)) { min(n) = w; p(n) = i; h = h insert (w, n) }
        }
      }
    }

    (min, p)
  }

  def dijkstraQ[E,V](g: LGraph[E,V])(start: Int, weight: E ⇒ Long)
    : (Array[Long],Array[Int]) = {

    val min = Array.fill(g.order)(Long.MaxValue)
    val visited = Array.fill(g.order)(false)
    val p = Array.fill(g.order)(-1)
    val q = PriorityQueue((0L, start))
    min(start) = 0L

    while (! q.isEmpty) {
      val (l, i) = q.dequeue
      if (!visited(i)) {
        visited(i) = true
        g neighbors i foreach { n ⇒ 
          val w = l + weight(g eLabel Edge(i, n))
          if (w < min(n)) { min(n) = w; p(n) = i; q.enqueue((w, n)) }
        }
      }
    }

    (min, p)
  }

  def dijkstraJQ[E,V](g: LGraph[E,V])(start: Int, weight: E ⇒ Long)
    : (Array[Long],Array[Int]) = {
    type In = (Long, Int)
    val comp = new java.util.Comparator[In]{
      def compare(a: In, b: In) = a._1 compare b._1
    }

    val min = Array.fill(g.order)(Long.MaxValue)
    val visited = Array.fill(g.order)(false)
    val p = Array.fill(g.order)(-1)
    val q = new JQueue(11, comp)

    q.offer((0L, start))
    min(start) = 0L

    while (! q.isEmpty) {
      val (l, i) = q.poll
      if (!visited(i)) {
        visited(i) = true
        g neighbors i foreach { n ⇒ 
          val w = l + weight(g eLabel Edge(i, n))
          if (w < min(n)) { min(n) = w; p(n) = i; q.offer((w, n)) }
        }
      }
    }

    (min, p)
  }
}

// vim: set ts=2 sw=2 et:
