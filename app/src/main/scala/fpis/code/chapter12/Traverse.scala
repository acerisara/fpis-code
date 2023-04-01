package fpis.code.chapter12

import fpis.code.chapter10.{Foldable, Monoid}
import fpis.code.chapter11.Functor

import scala.language.implicitConversions

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  type Const[M, _] = M

  implicit def monoidApplicative[M](M: Monoid[M]): Applicative[
    ({
      type f[x] = Const[M, x]
    })#f
  ] = new Applicative[({ type f[x] = Const[M, x] })#f] {
    def unit[A](a: => A): M = M.zero

    def map2[A, B, C](m1: M, m2: M)(f: (A, B) => C): M = M.op(m1, m2)
  }

  def foldMap[A, M](as: F[A])(f: A => M)(mb: Monoid[M]): M =
    traverse[({ type f[x] = Const[M, x] })#f, A, Nothing](as)(f)(
      monoidApplicative(mb)
    )
}

object Traverse {

  case class Tree[+A](head: A, tail: List[Tree[A]])

  val listTraverse: Traverse[List] = new Traverse[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)
    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
    override def toList[A](fa: List[A]): List[A] = fa
  }

  val optionTraverse: Traverse[Option] = new Traverse[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)
    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
    override def toList[A](fa: Option[A]): List[A] = fa.toList
  }

  val treeTraverse: Traverse[Tree] = new Traverse[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
      Tree(f(fa.head), fa.tail.map(map(_)(f)))
    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = ???
    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = ???
    override def toList[A](fa: Tree[A]): List[A] = ???
  }

}
