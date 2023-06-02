package fpis.code.chapter8

case class SGen[+A](forSize: Int => Gen[A]) {

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(n => forSize(n).flatMap(a => f(a).forSize(n)))

}

object SGen {

  def listOf[B](g: Gen[B]): SGen[List[B]] =
    SGen(n => Gen.listOfN(n, g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n max 1))

}
