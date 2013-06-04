package graph

import scala.collection.BitSet
import scalaz._, Scalaz._

/**
  * An unlabeled, undirected graph. Vertices are represented by integers
  * while edges are pairs of integers wrapped in a somewhat specialized
  * trait Edge.
  *
  * The graph has no explicit set of vertices. The graph contains all
  * vertices from 0 until its order.
  *
  * @author Stefan Höck
  */
sealed trait Graph {
  import Graph._

  /**
    * Returns the graph's adjacencyList, which is a mapping
    * from a vertex to its neighbors.
    */
  private[graph] lazy val adjacencyList: Array[List[Int]] =
    Graph adjacencyList this

  /**
    * Adds a new, isolated vertex to the graph
    */
  def addVertex: Graph = GraphImpl(order + 1, edges)

  /**
    * Adds a new, pendant vertex to the graph, which
    * is connected to vertex 'neighbor'.
    */
  def addVertex(neighbor: Int): Graph =
    addEdge(Edge(neighbor, order))

  /**
    * Adds a new vertex to the graph, which
    * is connected to the vertices given by 'neighbors'.
    */
  def addVertex(neighbors: Set[Int]): Graph =
    addEdges(neighbors map (Edge(_, order)))

  /**
    * Adds a new edge to the graph. If one of the vertices
    * connected by the edge is larger than the graph's order
    * the new graph's order is increased accordingly.
    */
  def addEdge(e: Edge): Graph =
    GraphImpl(order max (e.b + 1), edges + e)

  /**
    * Adds a several new edges to the graph. If one of the vertices
    * connected by these edges is larger than the graph's order
    * the new graph's order is increased accordingly.
    */
  def addEdges(es: Set[Edge]): Graph =
    GraphImpl(es.foldLeft(order)((o,e) ⇒ o max (e.b + 1)), edges | es)

  /**
    * Returns the number of neighbors of vertex v.
    */
  def degree(v: Int): Int = degrees(v)

  /** Returns the degrees of all vertices in an array. */
  private[graph] lazy val degrees: Array[Int] =
    adjacencyList map { _.size }

  /**
    * A Set of all edges in this graph
    */
  def edges: Set[Edge]

  /**
    * True if v is an isolated vertex (= has no neighbor)
    */
  def isIsolate(v: Int): Boolean = degree(v) == 0

  /**
    * True if v is a pendant vertex (= has exactly one neighbor)
    */
  def isPendant(v: Int): Boolean = degree(v) == 1

  /**
    * Returns an (unsorted) list of neighbors for vertex v.
    */
  def neighbors(v: Int): List[Int] = adjacencyList(v)

  /**
    * Returns the order (= number of vertices) of the graph
    */
  def order: Int

  /**
    * Returns the size (= number of edges) of the graph
    */
  lazy val size: Int = edges.size

  /**
    * Removes an edge from the graph
    */
  def removeEdge(e: Edge): Graph = GraphImpl (order, edges - e)

  /**
    * Removes a vertex from the graph, deleting all edges
    * connected to that vertex as well. All vertices (and 
    * the edges connecting them) larger than v are reduced
    * by one.
    */
  def removeVertex(v: Int): Graph = {
    def filtered = edges filterNot (_ connects v)
    def adjustVertex (x: Int) = if (x > v) x - 1 else x
    def adjustEdge(e: Edge) = Edge(adjustVertex (e.a), adjustVertex (e.b))

    if (v < order) GraphImpl(order - 1, filtered map adjustEdge)
    else this
  }

  override def toString = {
    val header = s"Unlabeled Graph, order: $order"
    val es = edges.toList.sorted mkString ", "

    s"$header; $es"
  }

  /**
    * Alias for addVertex
    */
  def :+ (neighbor: Int) = addVertex(neighbor)

  /**
    * Alias for addEdge
    */
  def :+ (e: Edge) = addEdge(e)

  /**
    * Alias for addEdges
    */
  def ++ (es: Set[Edge]) = addEdges(es)

  /**
    * Alias for removeEdge
    */
  def - (e: Edge) = removeEdge(e)

  /**
    * Alias for removeVertex
    */
  def - (v: Int) = removeVertex(v)
}

object Graph {

  /**
    * The empty graph.
    */
  val empty: Graph = GraphImpl(0, Set.empty)

  /**
    * Creates a new graph from the given set of edges.
    * The graphs order is the highest vertex found in
    * any of the given edges.
    */
  def apply(es: Set[Edge]): Graph =
    if (es.isEmpty) empty
    else GraphImpl(es.max.b + 1, es)

  /**
    * Creates a new graph from the given order and set of edges.
    * If the set contains edges connecting to a higher vertex
    * than the given order, this will result in a runtime exception.
    */
  def apply(order: Int, edges: Set[Edge]): Graph = {
    val o = if (edges.isEmpty) -1 else edges.max.b
    require(order > o,
      s"Highest vertex in edges was $o, but order is only $order")

    GraphImpl(order, edges)
  }

  /**
    * Calculates the adjacencyList of a graph. This is
    * also available as a lazy field from class Graph itself.
    */
  def adjacencyList(g: Graph): Array[List[Int]] = {
    val res = Array.fill(g.order)(List[Int]())

    def addEdge (e: Edge) {
      res(e.a) = e.b :: res(e.a)
      res(e.b) = e.a :: res(e.b)
    }

    g.edges foreach addEdge

    res
  }

  private final case class GraphImpl(
      order: Int,
      edges: Set[Edge])
    extends Graph
}

// vim: set ts=2 sw=2 et:
