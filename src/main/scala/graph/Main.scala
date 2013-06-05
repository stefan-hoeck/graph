package graph

import com.google.caliper.Runner
import benchmark._
import scalaz._, Scalaz._

object Main {
  def main(args: Array[String]) {
    Runner.main(classOf[IsoBenchmark], args)
  }
}

// vim: set ts=2 sw=2 et:
