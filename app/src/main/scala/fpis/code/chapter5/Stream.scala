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

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _                    => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _          => z
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((h, t) => p(h) && t)

  def takeWhileF(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h, t) => if (p(h)) cons(h, t) else t)

  def headOptionF: Option[A] = foldRight(None: Option[A])((h, _) => Some(h))

  def map[B](p: A => B): Stream[B] =
    foldRight(empty: Stream[B])((h, t) => cons(p(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A])((h, t) => if (p(h)) cons(h, t) else t)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](p: A => Stream[B]): Stream[B] =
    foldRight(empty: Stream[B])((h, t) => p(h).append(t))

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
