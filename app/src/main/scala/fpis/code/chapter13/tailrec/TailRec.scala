package fpis.code.chapter13.tailrec

import fpis.code.chapter11.Monad

sealed trait TailRec[A] {

  def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(this, f)

  def map[B](f: A => B): TailRec[B] = flatMap(f andThen (Return(_)))

}

case class Return[A](a: A) extends TailRec[A]
case class Suspend[A](resume: () => A) extends TailRec[A]
case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

object TailRec extends Monad[TailRec] {
  def unit[A](a: => A): TailRec[A] = Return(a)

  override def flatMap[A, B](fa: TailRec[A])(f: A => TailRec[B]): TailRec[B] =
    fa flatMap f

  def apply[A](a: => A): TailRec[A] = unit(a)

  @annotation.tailrec
  def run[A](tailRec: TailRec[A]): A = tailRec match {
    case Return(a)  => a
    case Suspend(r) => r()
    case FlatMap(x, f) =>
      x match {
        case Return(a)     => run(f(a))
        case Suspend(r)    => run(f(r()))
        case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
      }
  }

  def forever[A, B](tailRec: TailRec[A]): TailRec[B] = {
    lazy val t: TailRec[B] = forever(tailRec)
    tailRec flatMap (_ => t)
  }

}
