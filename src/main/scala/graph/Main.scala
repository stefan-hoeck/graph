package graph

import com.google.caliper.Runner
import benchmark._
import scalaz._, Scalaz._

object Main {
  def main(args: Array[String]) {
    Runner.main(classOf[IsoBenchmark], args)
  }
//  def main(args: Array[String]) {
//    val order = 7
//    val gs = test.samples allOfOrder order
//    val ps = Permutation ofSize order toList
//
//    proove7(gs, ps)
//  }
//
//  def lucky7(gs: List[Graph]) {
//    val start = System.currentTimeMillis
//    val size = gs.par.map(iso2.solve).size
//    val stop = System.currentTimeMillis
//    println(s"Solved: $size in ${stop - start} ms")
//  }
//
//  def proove7(gs: List[Graph], ps: List[Permutation]) {
//    var count = 0
//
//    implicit val LOrder = Order[Set[Edge]].toScalaOrdering
//
//    def canonicalSet(g: Graph) = iso2.canonize(g).edges
//
//    def transformCanonical(es: Set[Edge])(p: Permutation) =
//      es map p.mapEdge
//
//    def testPair(p: (Set[Edge], List[Graph])) = p match {
//      case (es, gs) ⇒ {
//        count += 1
//        val sets = (ps map transformCanonical(es)).toSet
//        val graphSets = gs map (_.edges) toSet
//
//        val res = sets ≟ graphSets
//        if(! res) {
//          println("sets")
//          sets foreach { s ⇒ println(s.toList.sorted) }
//
//          println("graphs")
//          graphSets foreach { s ⇒ println(s.toList.sorted) }
//        }
//
//        if (count % 100 == 0) println(s"Tested $count")
//
//        res
//      }
//    }
//
//    println("Generating pairs")
//
//    val pairs = gs groupBy canonicalSet
//
//    println("Pairs generated")
//    println(s"Start testing: ${pairs.size} to be tested")
//
//    pairs.toList ∀ testPair
//  }
}

// vim: set ts=2 sw=2 et:
