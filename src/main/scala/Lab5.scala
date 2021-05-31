import scala.language.implicitConversions

trait CommutativeMonoid[A]{
  def summ(a1:A, a2:A): A
  def zero: A
}
object CommutativeMonoid{
  implicit val mapCommutativeMonoid = new CommutativeMonoid[Map[String, Double]] {
    override def summ(x: Map[String, Double], y: Map[String, Double]):
    Map[String, Double] = x ++ y
    override val zero : Map[String, Double] = Map()
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