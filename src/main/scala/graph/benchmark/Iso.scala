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
  @Param(Array("10", "100", "500")) val size = 0
  var chains: List[Graph] = Nil
  var rings: List[Graph] = Nil

  override def setUp() {
    chains = graph.test.samples.chains(size)
    rings = graph.test.samples.rings(size)
  }

  def timeRefineChains(n: Int) = run(n) {
    chains.tail.tail foreach { iso.refine }
  }

  def timeRefineRings(n: Int) = run(n) {
    chains.tail.tail foreach { iso.refine }
  }

  def timeRefineChains2(n: Int) = run(n) {
    chains.tail.tail foreach { iso2.refine }
  }

  def timeRefineRings2(n: Int) = run(n) {
    chains.tail.tail foreach { iso2.refine }
  }
}

// vim: set ts=2 sw=2 et:
