import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import CommutativeMonoidOps._

object MonoidSpecification extends Properties("CommutativeMonoid") {
  property("associativity") = forAll {
    (a: Map[String, Double], b: Map[String, Double], c: Map[String, Double]) =>
      var s1 : Map[String, Double] = (a ++ b) ++ c
      var s2 : Map[String, Double] = a ++ (b ++ c)
      s1 == s2
  }
  property("commutability") = forAll {
    (a: Map[String, Double], b: Map[String, Double]) =>
      var s1 : Map[String, Double] = a ++ b
      var s2 : Map[String, Double] = b ++ a
      s1 == s2
  }
  property("neutral") = forAll {
    (a: Map[String, Double]) =>
      val z : Map[String, Double] = CommutativeMonoid.mapCommutativeMonoid.zero
      var s1 : Map[String, Double] = a ++ z
      var s2 : Map[String, Double] = z ++ a
      s1 == a
      s2 == a
  }
}