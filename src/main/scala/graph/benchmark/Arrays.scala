package graph.benchmark

import graph.Edge
import com.google.caliper.Param

class ArrayBenchmark extends BMark {
  @Param(Array("10", "100", "1000", "10000")) val size = 0

  var edges: List[Edge] = Nil

  override def setUp() {
    edges = (1 to size) map { Edge(_, 0) } toList
  }

  def timeCreateArray(n: Int) = run(n){ edges.toArray }

  def timeCreateIxSq(n: Int) = run(n){ edges.toVector }

  def timeLookupArray(n: Int) = run(n){ 
    val arr = edges.toArray
    var e: Edge = Edge(1, 0)

    0 until size foreach { i ⇒ e = arr(i) }
  }

  def timeLookupIxSq(n: Int) = run(n){
    val is = edges.toVector
    var e: Edge = Edge(1, 0)

    0 until size foreach { i ⇒ e = is(i) }
  }
}

// vim: set ts=2 sw=2 et:
