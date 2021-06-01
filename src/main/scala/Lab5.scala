import scala.language.{implicitConversions, postfixOps}

trait CommutativeMonoid[A] {
  def zero: A
  def summ(a: A, b: A): A
}

object CommutativeMonoid {
  implicit val mapCommutativeMonoid = new CommutativeMonoid[Map[String, Double]] {
    override def zero: Map[String, Double] = Map()

    override def summ(a: Map[String, Double], b: Map[String, Double]): Map[String, Double] =
      (a.keySet ++ b.keySet) map { k =>
        (k, a.getOrElse(k, 0.0) + b.getOrElse(k, 0.0))
      } toMap
  }
}
class CommutativeMonoidOps[A](x: A)(implicit g: CommutativeMonoid[A]) {
  def ++(y: A): A = g.summ(x, y)
}

object CommutativeMonoidOps {
  def main(args: Array[String]): Unit = {
    implicit def common[A](x: A)(implicit g: CommutativeMonoid[A]): CommutativeMonoidOps[A] = {
      new CommutativeMonoidOps[A](x)
    }
  }
}