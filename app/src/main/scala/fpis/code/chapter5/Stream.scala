package fpis.code.chapter5

import fpis.code.chapter5.Stream.{cons, empty}

sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty      => None
    case Cons(h, _) => Some(h()) // Explicit forcing of the h thunk using h()
  }

  // This implementation is not stack-safe
  def toList: List[A] = this match {
    case Empty      => List.empty
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _                   => empty
  }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

}
