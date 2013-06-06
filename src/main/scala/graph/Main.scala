package graph

import com.google.caliper.Runner
import benchmark._
import scalaz._, Scalaz._

object Main {
  def main(args: Array[String]) {
    Runner.main(classOf[IsoBenchmark], args)
  }
//  def main(args: Array[String]) {
//    val gs = test.samples allOfOrder 7 take 500000
//    1 to 10 foreach { _ ⇒ lucky7(gs) }
//  }
//
//  def lucky7(gs: List[Graph]) {
//    val start = System.currentTimeMillis
//    val size = gs.par.map(iso2.solve).size
//    val stop = System.currentTimeMillis
//    println(s"Solved: $size in ${stop - start} ms")
//  }
}

// vim: set ts=2 sw=2 et:
