package graph.benchmark

import graph._
import com.google.caliper.Param

/** Better to create new ds in method `shatter` than have a shared
  * one and update it with every iteration
  *
  * Mutable vs. immutable BitSets:
  * Both perform equally well for smaller molecules (size = 10, 100) but
  * mutable ones are about twice as fast for larger molecules (size = 500)
  * This was tested for refinement only. When we have to make clones of
  * mutable bitsets, immutable might look even more favorable.
  *
  */
class IsoBenchmark extends BMark {
  var gs: List[Graph] = Nil

  override def setUp() {
    gs = test.samples allOfOrder 6
  }

  def timeSolve(n: Int) = run(n) { gs.par map iso2.solve size }
}

// vim: set ts=2 sw=2 et:
