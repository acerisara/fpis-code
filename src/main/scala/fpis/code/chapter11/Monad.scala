package fpis.code.chapter11

import fpis.code.chapter12.{Applicative, Traverse}
import fpis.code.chapter6.State
import fpis.code.chapter7.Par
import fpis.code.chapter7.Par.Par
import fpis.code.chapter8.Gen
import fpis.code.chapter9.DefaultParser.Parser
import fpis.code.chapter9.Parsers

trait Monad[F[_]] extends Applicative[F] {

  /** Minimal set of primitives:
    *
    *   - flatMap, unit
    *   - compose, unit
    *   - map, unit and join
    *
    * A minimal implementation of Monad must implement unit and override either
    * flatMap or join and map.
    */

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))

  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa)

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a =>
    flatMap(f(a))(g)

}

object Monad {

  val genMonad: Monad[Gen] = new Monad[Gen] {
    override def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] =
      fa.flatMap(f)
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)
    override def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] =
      Par.flatMap(fa)(f)
  }

  def parserMonad(p: Parsers[Parser]): Monad[Parser] = new Monad[Parser] {
    override def unit[A](a: => A): Parser[A] = p.succeed(a)
    override def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] =
      p.flatMap(fa)(f)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa.flatMap(f)
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
      fa.flatMap(f)
  }

  val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)
    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = fa.flatMap(f)
  }

  def eitherMonad[E]: Monad[({ type f[x] = Either[E, x] })#f] = new Monad[
    ({
      type f[x] = Either[E, x]
    })#f
  ] {
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](
        fa: Either[E, A]
    )(f: A => Either[E, B]): Either[E, B] = fa match {
      case Left(e)  => Left(e)
      case Right(a) => f(a)
    }
  }

  // Not stack-safe
  val function0Monad: Monad[Function0] = new Monad[Function0] {
    override def unit[A](a: => A): () => A = () => a
    override def flatMap[A, B](a: () => A)(f: A => () => B): () => B = () =>
      f(a())()
  }

  def stateMonad[S]: Monad[
    ({
      type f[x] = State[S, x]
    })#f
  ] = new Monad[({ type f[x] = State[S, x] })#f] {
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](st: State[S, A])(
        f: A => State[S, B]
    ): State[S, B] = st.flatMap(f)
  }

  val F: Monad[
    ({
      type f[x] = State[Int, x]
    })#f
  ] = stateMonad[Int]

  def zipWithIndex[A](as: List[A]): List[(Int, A)] =
    as.foldLeft(F.unit(List[(Int, A)]()))((acc, a) =>
      for {
        xs <- acc
        n <- State.get[Int]
        _ <- State.set(n + 1)
      } yield (n, a) :: xs
    ).run(0)
      ._1
      .reverse

  def compose[F[_], G[_]](
      F: Monad[F],
      G: Monad[G]
  ): Monad[({ type f[x] = F[G[x]] })#f] = new Monad[
    ({
      type f[x] = F[G[x]]
    })#f
  ] {
    override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))

    // This can't be written generically, one must be traversable (see `composeM` below)
    override def join[A](ffa: F[G[F[G[A]]]]): F[G[A]] = ???
  }

  def composeM[F[_], G[_]](
      F: Monad[F],
      G: Monad[G],
      T: Traverse[G]
  ): Monad[({ type f[x] = F[G[x]] })#f] = new Monad[
    ({
      type f[x] = F[G[x]]
    })#f
  ] {
    override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))

    override def flatMap[A, B](fa: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
      F.flatMap(fa) { ga =>
        val g = G.map(ga)(f)
        // G.map(ga)(f) is G[F[G[B]]] and cannot be implemented in a general way,
        // but this is turning G[F[G[B]]] into F[G[G[B]]] where the inner G layers can be flatten with join
        val t = T.traverse(ga)(f)(F)
        F.map(t)(G.join)
      }
  }

}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}
