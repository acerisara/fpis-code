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
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] = as match {
    case Nil         => Nil
    case Cons(_, xs) => xs
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

}
