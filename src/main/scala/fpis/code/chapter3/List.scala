package fpis.code.chapter3

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil         => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    as.reverse.foldLeft(Nil: List[A])((b, a) => Cons(a, b))

  def tail[A](as: List[A]): List[A] = as match {
    case Nil         => Nil
    case Cons(_, xs) => xs
  }

  def headOr[A](as: List[A], default: => A): A = as match {
    case Nil        => default
    case Cons(x, _) => x
  }

  def setHead[A](x: A, as: List[A]): List[A] = as match {
    case Nil         => Nil
    case Cons(_, xs) => Cons(x, xs)
  }

  @tailrec
  def drop[A](n: Int, as: List[A]): List[A] = as match {
    case Nil         => Nil
    case Cons(_, xs) => if (n > 0) drop(n - 1, xs) else as
  }

  @tailrec
  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _                   => as
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil        => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def init[A](as: List[A]): List[A] = as match {
    case Nil          => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs)  => Cons(x, init(xs))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil         => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def foldRightFL[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    // [1,2,3,4]
    // foldLeft: ((((0 + 1) + 2) + 3) + 4)
    // foldRight: (1 + (2 + (3 + (4 + 0))))

    // reverse: [4,3,2,1]
    // foldLeft: (((((0 + 4) + 3) + 2) + 1)
    // This implementation maintains the correct associativity
    foldLeft(reverse(as), z)((b, a) => f(a, b))
  }

  def sumFR(ints: List[Int]): Int = foldRight(ints, 0)(_ + _)

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, b) => b + 1)

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil         => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def foldLeftFR[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(as), z)((a, b) => f(b, a))

  def sumFL(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def productFL(ds: List[Double]): Double = foldLeft(ds, 0.0)(_ * _)

  def lengthFL[A](as: List[A]): Int = foldLeft(as, 0)((b, _) => b + 1)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((b, a) => Cons(a, b))

  def appendFL[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(reverse(a1), a2)((b, a) => Cons(a, b))

  def appendFR[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  def flatten[A](ls: List[List[A]]): List[A] =
    foldRight(ls, Nil: List[A])(append)

  def plus1(ints: List[Int]): List[Int] = ints match {
    case Nil         => Nil
    case Cons(x, xs) => Cons(x + 1, plus1(xs))
  }

  def doubleToString(ds: List[Double]): List[String] = ds match {
    case Nil         => Nil
    case Cons(x, xs) => Cons(x.toString, doubleToString(xs))
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil         => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil                 => Nil
    case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
    case Cons(_, xs)         => filter(xs)(f)
  }

  def plus1FR(ints: List[Int]): List[Int] =
    foldRight(ints, Nil: List[Int])((a, b) => Cons(a + 1, b))

  def doubleToStringFR(ds: List[Double]): List[String] =
    foldRight(ds, Nil: List[String])((a, b) => Cons(a.toString, b))

  def mapFR[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((a, b) => Cons(f(a), b))

  def filterFR[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((a, b) => append(f(a), b))

  def filterFM[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def zipInts(i1: List[Int], i2: List[Int]): List[Int] =
    (i1, i2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(i1, is1), Cons(i2, is2)) =>
        Cons(i1 + i2, zipInts(is1, is2))
    }

  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] =
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(x1, xs1), Cons(x2, xs2)) =>
        Cons(f(x1, x2), zipWith(xs1, xs2)(f))
    }

  @tailrec
  def startsWith[A](as: List[A], prefix: List[A]): Boolean =
    (as, prefix) match {
      case (_, Nil)                                   => true
      case (Cons(x1, xs1), Cons(x2, xs2)) if x1 == x2 => startsWith(xs1, xs2)
      case _                                          => false
    }

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    (sup, sub) match {
      case (Cons(_, _), Cons(_, _)) if startsWith(sup, sub) => true
      case (Cons(_, xs1), _) => hasSubsequence(xs1, sub)
      case _                 => false
    }
}
