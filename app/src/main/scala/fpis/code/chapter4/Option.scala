package fpis.code.chapter4

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] =
    this match {
      case None    => None
      case Some(x) => Some(f(x))
    }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B =
    this match {
      case None    => default
      case Some(x) => x
    }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    flatMap { a =>
      if (f(a)) this else None
    }

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
    for {
      a <- oa
      b <- ob
    } yield f(a, b)

  def sequence[A](oas: List[Option[A]]): Option[List[A]] =
    oas.foldLeft(Some(List.empty[A]): Option[List[A]])((oas, oa) => {
      for {
        as <- oas
        a <- oa
      } yield as :+ a
    })

}
