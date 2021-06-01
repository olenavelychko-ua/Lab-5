import org.scalacheck.Properties
import org.scalacheck.Prop.forAll


object MonoidSpecification extends Properties("CommutativeMonoid") {
  property("associativity Summ") = forAll {
    (a: Map[String, Double], b: Map[String, Double], c: Map[String, Double]) =>
      var s1: Map[String, Double] = CommutativeMonoid.mapCommutativeMonoid.summ(a, CommutativeMonoid.mapCommutativeMonoid.summ(b, c))
      var s2: Map[String, Double] = CommutativeMonoid.mapCommutativeMonoid.summ(b, CommutativeMonoid.mapCommutativeMonoid.summ(a, c))
      s1.equals(s2)
  }
  property("associativity FoldLeft") = forAll {
    (a: Map[String, Double], b: Map[String, Double], c: Map[String, Double]) =>

      val list1 = List(a, b, c)
      val s1 = list1.foldLeft(CommutativeMonoid.mapCommutativeMonoid.zero)(CommutativeMonoid.mapCommutativeMonoid.summ)
      val list2 = List(b, a, c)
      var s2 = list2.foldLeft(CommutativeMonoid.mapCommutativeMonoid.zero)(CommutativeMonoid.mapCommutativeMonoid.summ)
      s1.equals(s2)
  }
  property("commutability summ") = forAll {
    (a: Map[String, Double], b: Map[String, Double]) =>
      var s1: Map[String, Double] = CommutativeMonoid.mapCommutativeMonoid.summ(a, b)
      var s2: Map[String, Double] = CommutativeMonoid.mapCommutativeMonoid.summ(b, a)
      s1.equals(s2)
  }
  property("commutability FoldLeft") = forAll {
    (a: Map[String, Double], b: Map[String, Double]) =>
      val list1 = List(a, b)
      val s1 = list1.foldLeft(CommutativeMonoid.mapCommutativeMonoid.zero)(CommutativeMonoid.mapCommutativeMonoid.summ)
      val list2 = List(b, a)
      var s2 = list2.foldLeft(CommutativeMonoid.mapCommutativeMonoid.zero)(CommutativeMonoid.mapCommutativeMonoid.summ)
      s1.equals(s2)
  }
  property("neutral") = forAll {
    (a: Map[String, Double]) =>
      val z: Map[String, Double] = CommutativeMonoid.mapCommutativeMonoid.zero
      var s1: Map[String, Double] = CommutativeMonoid.mapCommutativeMonoid.summ(a, z)
      var s2: Map[String, Double] = CommutativeMonoid.mapCommutativeMonoid.summ(z, a)
      s1.equals(a)
      s2.equals(a)
  }
}