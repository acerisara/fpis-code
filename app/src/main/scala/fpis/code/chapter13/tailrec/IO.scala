package fpis.code.chapter13.tailrec

import fpis.code.chapter11.Monad

sealed trait IO[A] {

  def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
  def map[B](f: A => B): IO[B] = flatMap(f andThen (Return(_)))

}

case class Return[A](a: A) extends IO[A]
case class Suspend[A](resume: () => A) extends IO[A]
case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]

object IO extends Monad[IO] {
  def unit[A](a: => A): IO[A] = Return(a)

  override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] =
    fa flatMap f

  def apply[A](a: => A): IO[A] = unit(a)

  @annotation.tailrec
  def run[A](tailRec: IO[A]): A = tailRec match {
    case Return(a)  => a
    case Suspend(r) => r()
    case FlatMap(x, f) =>
      x match {
        case Return(a)     => run(f(a))
        case Suspend(r)    => run(f(r()))
        case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
      }
  }

  def forever[A, B](tailRec: IO[A]): IO[B] = {
    lazy val t: IO[B] = forever(tailRec)
    tailRec flatMap (_ => t)
  }

}
