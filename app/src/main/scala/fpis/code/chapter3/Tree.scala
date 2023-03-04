package fpis.code.chapter3

import scala.{List => SList}

sealed trait Tree[+A]

case class Leaf[+A](value: A) extends Tree[A]

case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def toList[A](t: Tree[A]): SList[A] = t match {
    case Leaf(a) => SList(a)
    case Branch(l, r) => toList(l) ++ toList(r)
  }

  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(_)      => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

  def maximum(t: Tree[Int]): Int =
    t match {
      case Leaf(n)      => n
      case Branch(l, r) => maximum(l) max maximum(r)
    }

  def depth[A](t: Tree[A]): Int =
    t match {
      case Leaf(_)      => 0
      case Branch(l, r) => 1 + depth(l) max depth(r)
    }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(v)      => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B =
    t match {
      case Leaf(v)      => f(v)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

  def sizeF[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ + _)

  def maximumF(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)

  def depthF[A](t: Tree[A]): Int = fold(t)(_ => 0)((b1, b2) => 1 + (b1 max b2))

  def mapF[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

}
