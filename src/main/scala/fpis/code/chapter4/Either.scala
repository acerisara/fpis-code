package fpis.code.chapter4

trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] =
    this match {
      case Left(e)  => Left(e)
      case Right(v) => Right(f(v))
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e)  => Left(e)
      case Right(v) => f(v)
    }

  def orElse[EE >: E, B >: A](default: => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(_)  => default
      case Right(_) => this
    }

  def map2[EE >: E, B, C](e2: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a <- this
      b <- e2
    } yield f(a, b)

}

object Either {

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldLeft(Right(List.empty[A]): Either[E, List[A]])((es, e) =>
      es.map2(e)(_ :+ _)
    )

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldLeft(Right(List.empty[B]): Either[E, List[B]])((es, a) =>
      es.map2(f(a))(_ :+ _)
    )

  def sequenceT[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(e => e)

}

case class Left[+EE](value: EE) extends Either[EE, Nothing]

case class Right[+AA](value: AA) extends Either[Nothing, AA]
