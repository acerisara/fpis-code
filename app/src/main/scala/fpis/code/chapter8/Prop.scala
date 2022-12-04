package fpis.code.chapter8

import fpis.code.chapter8.Prop.{FailedCase, SuccessCount}

trait Prop {

  def check: Either[(FailedCase, SuccessCount), SuccessCount]

  def &&(p: Prop): Prop = new Prop {
    override def check: Either[(FailedCase, SuccessCount), SuccessCount] =
      (this.check, p.check) match {
        case (Right(s1), Right(s2)) => Right(s1 + s2)
        case (Right(s), Left(e))    => Left((e._1, e._2 + s))
        case (Left(e), Right(s))    => Left((e._1, e._2 + s))
        case (Left(e1), Left(e2))   => Left((e1._1, e1._2 + e2._2))
      }
  }

}

object Prop {

  type FailedCase = String
  type SuccessCount = Int

}
