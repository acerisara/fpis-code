package fpis.code.chapter8

case class SGen[+A](g: Int => Gen[A]) {

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val gb: Int => Gen[B] = n => {
      g(n).flatMap {
        f(_).g(n)
      }
    }

    SGen(gb)
  }

}

object SGen {

  def listOf[B](g: Gen[B]): SGen[List[B]] = SGen(g.listOfN)

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(_ => g.listOfN(1))

}
