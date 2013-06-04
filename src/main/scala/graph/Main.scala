package graph

import com.google.caliper.Runner
import benchmark._
import scalaz._, Scalaz._

object Main {

  def main(args: Array[String]) {
    //Runner.main(classOf[IsoBenchmark], args)
    for (i ← 1 to 10) {
      val start = System.currentTimeMillis

      val s = (test.samples allOfOrder 7).par map iso2.solve size

      val stop = System.currentTimeMillis

      println(s"$s: ${stop - start}")
    }
  }

}

// vim: set ts=2 sw=2 et:
