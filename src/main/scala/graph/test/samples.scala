package graph.test

import graph.{Graph, Edge}
import scalaz._, Scalaz._

/** A couple of sample graphs defined manually */
object samples {
  val empty = Graph.empty
  val single = Graph.empty.addVertex

  def allOfOrder(n: Int): List[Graph] = {
    val MaxA = n - 1
    val MaxB = n - 2

    def enum(a: Int, b: Int, es: Set[Edge]): List[Graph] = {
      val newEs = (a > b) ? List(es, es + Edge(a, b)) | Nil

      a match {
        case MaxA ⇒ newEs map { Graph(n, _) }
        case `b`  ⇒ Nil
        case _    ⇒ newEs flatMap { es ⇒ 
          enum(a, b + 1, es) ::: enum(a + 1, b, es)
        }
      }
    }

    if (n <= 0) List(Graph.empty) else enum(1, 0, Set.empty)
  }

  /** linear graphs (chains) of order 1 to n */
  def chains(n: Int): List[Graph] = n match {
    case 1            ⇒ List(single)
    case x if x < 1 ⇒ Nil
    case x ⇒ List.range(0, n - 1).scanLeft(single)(_ :+ _)
  }

  /** linear graphs (chains) of order 1 to 100 */
  def chains100: List[Graph] = chains(100)

  def complex: Graph = Graph(Set(
    Edge(0, 1),
    Edge(1, 2),
    Edge(1, 3),
    Edge(3, 4),
    Edge(3, 8),
    Edge(4, 5),
    Edge(4, 6),
    Edge(5, 6),
    Edge(5, 9),
    Edge(6, 7),
    Edge(7, 8)
  ))

  /** cyclic graphs */
  def rings(n: Int): List[Graph] = n match {
    case x if x < 3 ⇒ Nil
    case x ⇒ chains(n).tail.tail map { g ⇒ g :+ Edge(0, g.order - 1) }
  }

  /** cycles of order 3 to 100 */
  def rings100: List[Graph] = rings(100)
}

// vim: set ts=2 sw=2 et:
