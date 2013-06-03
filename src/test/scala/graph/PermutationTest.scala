package graph

import org.scalacheck._, Prop._
import scalaz._, Scalaz._
import scalaz.scalacheck.ScalazProperties.{equal, monoid}

object PermutationTest extends Properties("Permutation") {

  val perms: List[Permutation] = Permutation ofSize 7 toList

  implicit val PEqual: Equal[Permutation] = new Equal[Permutation] {
    def equal(a: Permutation, b: Permutation) =
      (0 to 6 toList) ∀ { i ⇒ a(i) ≟ b(i) }
  }

  implicit val PArbitrary: Arbitrary[Permutation] =
    Arbitrary(Gen oneOf perms)

  property("equal") = equal.laws[Permutation]

  property("monoid") = monoid.laws[Permutation]

  property("inverse") = forAll{ p: Permutation ⇒ 
    (p compose p.inverse) ≟ ∅[Permutation]
  }
}

// vim: set ts=2 sw=2 et:
