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

  def orElse[B >: A](default: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(default)

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

  def lift[A, B](f: A => B): Option[A] => Option[B] = oa => oa.map(f)

  def map2[A, B, C](o1: Option[A], o2: Option[B])(f: (A, B) => C): Option[C] =
    for {
      a <- o1
      b <- o2
    } yield f(a, b)

  def sequence[A](oas: List[Option[A]]): Option[List[A]] =
    oas.foldLeft(Some(List.empty[A]): Option[List[A]])((oas, oa) => {
      for {
        as <- oas
        a <- oa
      } yield as :+ a
    })

  def sequenceM[A](oas: List[Option[A]]): Option[List[A]] =
    oas.foldLeft(Some(List.empty[A]): Option[List[A]])((oas, oa) =>
      map2(oas, oa)(_ :+ _)
    )

  def sequenceP[A](oas: List[Option[A]]): Option[List[A]] =
    oas match {
      case Nil => Some(List.empty)
      case oa :: oas =>
        for {
          as <- sequenceP(oas)
          a <- oa
        } yield a :: as
    }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case _: Exception => None
    }

  // We want to map over a list with a function that might fail, returning None
  // if applying it to any element of the list returns None. The problem with this implementation is that
  // we have to iterate the list twice
  def parseInts(a: List[String]): Option[List[Int]] =
    sequence(a map (i => Try(i.toInt)))

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldLeft(Some(List.empty[B]): Option[List[B]]) { (oas, a) =>
      map2(oas, f(a))(_ :+ _)
    }

  def sequenceT[A](oas: List[Option[A]]): Option[List[A]] =
    traverse(oas)(oa => oa)

}
