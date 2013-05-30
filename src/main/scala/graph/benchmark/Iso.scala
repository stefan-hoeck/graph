package graph.benchmark

import graph._

/** Better to create new ds in method `shatter` than have a shared
  * one and update it with every iteration
  */
class Iso extends BMark {
  var chains: List[Graph] = Nil

  override def setUp() { chains = graph.test.samples.chains100 }

  def timeShatter(n: Int) = run(n) {
    var res: Array[Int] = null

    chains.tail.tail foreach { c ⇒ 
      val i = new iso.Iso(c)
      val cell = iso.Cell(0, c.order)
      i.shatter(cell, cell)
      i.shatter(i.c(2), i.c(0))

      res = i.p
    }
  }
}

// vim: set ts=2 sw=2 et:
