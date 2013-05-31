package graph.benchmark

import graph._
import com.google.caliper.Param

/** Better to create new ds in method `shatter` than have a shared
  * one and update it with every iteration
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
}

// vim: set ts=2 sw=2 et:
