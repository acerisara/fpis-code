package fpis.code.chapter13.async

import fpis.code.chapter11.Monad
import fpis.code.chapter7.Par
import fpis.code.chapter7.Par.Par

import scala.annotation.tailrec

sealed trait IO[A] {

  def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
  def map[B](f: A => B): IO[B] = flatMap(f andThen (Return(_)))

}

case class Return[A](a: A) extends IO[A]
case class Suspend[A](resume: Par[A]) extends IO[A]
case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]

object IO extends Monad[IO] {

  override def unit[A](a: => A): IO[A] = Return(a)

  override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] =
    fa flatMap f

  def apply[A](a: => A): IO[A] = unit(a)

  @tailrec
  def step[A](async: IO[A]): IO[A] = async match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f)     => step(f(x))
    case _                         => async
  }

  def run[A](async: IO[A]): Par[A] = step(async) match {
    case Return(a)  => Par.unit(a)
    case Suspend(r) => Par.flatMap(r)(a => run(Return(a)))
    case FlatMap(x, f) =>
      x match {
        case Suspend(r) => Par.flatMap(r)(a => run(f(a)))
        case _ => sys.error("Impossible: `step` eliminates these cases")
      }
  }

}
