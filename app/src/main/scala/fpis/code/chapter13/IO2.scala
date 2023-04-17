package fpis.code.chapter13

import fpis.code.chapter11.Monad

sealed trait IO2[A] {
  def flatMap[B](f: A => IO2[B]): IO2[B] = FlatMap(this, f)
  def map[B](f: A => B): IO2[B] = flatMap(f andThen (Return(_)))
}

case class Return[A](a: A) extends IO2[A]
case class Suspend[A](resume: () => A) extends IO2[A]
case class FlatMap[A, B](sub: IO2[A], k: A => IO2[B]) extends IO2[B]

object IO2 extends Monad[IO2] {
  def unit[A](a: => A): IO2[A] = Return(a)
  override def flatMap[A, B](fa: IO2[A])(f: A => IO2[B]): IO2[B] = fa flatMap f
  def apply[A](a: => A): IO2[A] = unit(a)

  @annotation.tailrec
  def run[A](io: IO2[A]): A = io match {
    case Return(a)  => a
    case Suspend(r) => r()
    case FlatMap(x, f) =>
      x match {
        case Return(a)     => run(f(a))
        case Suspend(r)    => run(f(r()))
        case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
      }
  }

  def forever[A, B](a: IO2[A]): IO2[B] = {
    lazy val t: IO2[B] = forever(a)
    a flatMap (_ => t)
  }

}
