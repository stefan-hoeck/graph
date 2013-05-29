package graph

import com.google.caliper.Runner
import benchmark._

object Main {
  def main(args: Array[String]) {
    Runner.main(classOf[Dijkstra], args)
  }
}

// vim: set ts=2 sw=2 et:
