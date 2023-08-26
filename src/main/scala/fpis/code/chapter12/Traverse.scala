package fpis.code.chapter12

import fpis.code.chapter10.{Foldable, Monoid}
import fpis.code.chapter11.Functor
import fpis.code.chapter11.Monad.stateMonad
import fpis.code.chapter12.Applicative.product
import fpis.code.chapter6.State

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

    override def map2[A, B, C](m1: M, m2: M)(f: (A, B) => C): M = M.op(m1, m2)
  }

  override def foldMap[A, M](as: F[A])(f: A => M)(mb: Monoid[M]): M = {
    traverse[({ type f[x] = Const[M, x] })#f, A, Nothing](as)(f) {
      monoidApplicative(mb)
    }
  }

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({ type f[x] = State[S, x] })#f, A, B](fa)(f)(stateMonad)

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) =>
      for {
        s1 <- State.get[S]
        (b, s2) = f(a, s1)
        _ <- State.set(s2)
      } yield b
    ).run(s)

  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, s) => (s.head, s.tail))._1

  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])(
      G: Applicative[G],
      H: Applicative[H]
  ): (G[F[B]], H[F[B]]) = traverse[({ type f[x] = (G[x], H[x]) })#f, A, B](fa) {
    a => (f(a), g(a))
  }(product(G, H))

}

object Traverse {

  case class Tree[+A](head: A, tail: List[Tree[A]])

  val listTraverse: Traverse[List] = new Traverse[List] {
    override def map[A, B](as: List[A])(f: A => B): List[B] = as.map(f)

    override def traverse[M[_], A, B](as: List[A])(f: A => M[B])(implicit
        M: Applicative[M]
    ): M[List[B]] =
      as.foldRight(M.unit(List.empty[B]))((a, mb) => M.map2(f(a), mb)(_ :: _))
  }

  val optionTraverse: Traverse[Option] = new Traverse[Option] {
    override def map[A, B](oa: Option[A])(f: A => B): Option[B] = oa.map(f)

    override def traverse[M[_], A, B](
        oa: Option[A]
    )(f: A => M[B])(implicit M: Applicative[M]): M[Option[B]] = oa match {
      case Some(v) => M.map(f(v))(Some(_))
      case None    => M.unit(None)
    }
  }

  val treeTraverse: Traverse[Tree] = new Traverse[Tree] {
    override def map[A, B](ta: Tree[A])(f: A => B): Tree[B] =
      Tree(f(ta.head), ta.tail.map(map(_)(f)))

    override def traverse[M[_], A, B](ta: Tree[A])(f: A => M[B])(implicit
        M: Applicative[M]
    ): M[Tree[B]] = {
      M.map2(f(ta.head), listTraverse.traverse(ta.tail)(traverse(_)(f)))(
        Tree(_, _)
      )
    }
  }

}
