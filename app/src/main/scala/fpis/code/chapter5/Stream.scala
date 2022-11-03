package fpis.code.chapter5

import fpis.code.chapter5.Stream.{cons, empty, unfold}

sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty      => None
    case Cons(h, _) => Some(h())
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

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

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

  def tail(): Stream[A] = this match {
    case Empty      => Empty
    case Cons(_, t) => t()
  }

  def mapU[B](p: A => B): Stream[B] =
    unfold(this)(s => s.headOption.map(a => p(a)).map((_, s.tail())))

  def takeU(n: Int): Stream[A] = unfold((this, n)) { case (s, n) =>
    if (n > 0) s.headOption.map(a => (a, (s.tail(), n - 1)))
    else None
  }

  def takeWhileU(p: A => Boolean): Stream[A] =
    unfold(this)(s =>
      s.headOption.flatMap(a => if (p(a)) Some(a, s.tail()) else None)
    )

  // TODO: Try implementing zip* with unfold

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    (this, s2) match {
      case (Empty, _) => Empty
      case (_, Empty) => Empty
      case (Cons(h1, t1), Cons(h2, t2)) =>
        cons(f(h1(), h2()), t1().zipWith(t2())(f))
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    (this, s2) match {
      case (Empty, Empty)      => Empty
      case (Cons(h, t), Empty) => cons((Some(h()), None), t().zipAll(s2))
      case (Empty, Cons(h, t)) => cons((None, Some(h())), zipAll(t()))
      case (Cons(h1, t1), Cons(h2, t2)) =>
        cons((Some(h1()), Some(h2())), t1().zipAll(t2()))
    }

  def startsWith[B >: A](s: Stream[B]): Boolean = (this, s) match {
    case (Cons(h1, t1), Cons(h2, t2)) if h1() == h2() => t1().startsWith(t2())
    case (_, Empty)                                   => true
    case _                                            => false
  }

  def tails: Stream[Stream[A]] = unfold(this) {
    case Cons(h, t) => Some(cons(h(), t()), t())
    case _          => None
  }.append(Stream(empty))

  def hasSubsequence[B >: A](s: Stream[B]): Boolean =
    tails.exists(_.startsWith(s))

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

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs(): Stream[Int] = {
    def go(current: Int, next: Int): Stream[Int] =
      cons(current, go(next, current + next))

    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None         => Stream.empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  val onesU: Stream[Int] = unfold(1)(s => Some(s, s))

  def constantU[A](a: A): Stream[A] = unfold(a)(s => Some(s, s))

  def fromU(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))

  def fibsU(): Stream[Int] = unfold((0, 1)) { case (current, next) =>
    Some(current, (next, current + next))
  }

}
