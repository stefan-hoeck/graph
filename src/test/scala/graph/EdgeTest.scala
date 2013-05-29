package graph

import scalaz._, Scalaz._, scalacheck.ScalaCheckBinding._
import org.scalacheck._, Prop._

/**
 * @author Stefan Höck
 */
object EdgeTest extends Properties ("Edge") {
  val edgeGen = for {
    a ← Gen choose (0, Int.MaxValue)
    b ← Gen choose (0, Int.MaxValue)
    if (a != b)
  } yield Edge(a,b)

  implicit val edgeArbitrary = Arbitrary (edgeGen)

  property ("equal") = Prop.forAll {es: (Edge,Edge) ⇒ 
    val (ea, eb) = es
    (ea ≟ eb) ≟ ((ea.a ≟ eb.a) && (ea.b ≟ eb.b))
  }

  property("order") = forAll {es: (Edge,Edge) ⇒ 
    val (ea, eb) = es
    (ea ?|? eb) ≟ (ea.compare(eb) ?|? 0)
  }

  property ("aSmallerb") = Prop.forAll {e: Edge ⇒ 
    e.a <= e.b
  }

  property ("isNeighbor") = Prop.forAll {ei: (Edge,Int) ⇒ 
    val (e, i) = ei
    (e connects i) ≟ ((e.a ≟ i) || (e.b ≟ i))
  }

  property ("compare") = Prop.forAll {es: (Edge,Edge) ⇒ 
    val (a,b) = es
    (if (a.b > b.b) a > b else true) :| "greater by a" &&
    (if (a.b < b.b) a < b else true) :| "smaller by a" &&
    (if (a.b ≟ b.b) (a.a compare b.a) ≟ (a compare b) else true) :| "by b"
  }
    
}

// vim: set ts=2 sw=2 et:
