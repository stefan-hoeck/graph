package graph.test

import graph.{Graph, Edge}
import scalaz._, Scalaz._

/** A couple of sample graphs defined manually */
object samples {
  val empty = Graph.empty
  val single = Graph.empty.addVertex

  /** linear graphs (chains) of order 1 to n */
  def chains(n: Int): List[Graph] = n match {
    case 1            ⇒ List(single)
    case x if x < 1 ⇒ Nil
    case x ⇒ List.range(0, n - 1).scanLeft(single)(_ :+ _)
  }

  /** linear graphs (chains) of order 1 to 100 */
  def chains100: List[Graph] = chains(100)

  /** cyclic graphs */
  def rings(n: Int): List[Graph] = n match {
    case x if x < 3 ⇒ Nil
    case x ⇒ chains(n).tail.tail map { g ⇒ g :+ Edge(0, g.order - 1) }
  }

  /** cycles of order 3 to 100 */
  def rings100: List[Graph] = rings(100)
}

// vim: set ts=2 sw=2 et:
