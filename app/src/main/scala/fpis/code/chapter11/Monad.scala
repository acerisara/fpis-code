package fpis.code.chapter11

import fpis.code.chapter7.Par
import fpis.code.chapter7.Par.Par
import fpis.code.chapter8.Gen
import fpis.code.chapter9.MyParser.Parser
import fpis.code.chapter9.Parsers

trait Monad[F[_]] extends Functor[F] {
  // Minimal sets of combinators:
  // 1. flatMap, unit
  // 2. compose, unit
  // 3. map, unit and join

  def unit[A](a: => A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  def sequence[A](lfa: List[F[A]]): F[List[A]] =
    lfa.foldLeft(unit(List.empty[A])) { (b, a) =>
      map2(b, a)(_ :+ _)
    }

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldLeft(unit(List.empty[B])) { (b, a) =>
      map2(b, f(a))(_ :+ _)
    }

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def filterM[A](la: List[A])(f: A => F[Boolean]): F[List[A]] =
    la.foldLeft(unit(List.empty[A])) { (b, a) =>
      map2(b, f(a)) { (as, include) =>
        if (include) as :+ a else as
      }
    }

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a =>
    flatMap(f(a))(g)

  def flatMapC[A, B](ma: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit) => ma, f)(())

  def joinF[A](mma: F[F[A]]): F[A] = flatMap(mma)(identity)

  def flatMapJ[A, B](ma: F[A])(f: A => F[B]): F[B] = joinF(map(ma)(f))

}

object Monad {

  val genMonad: Monad[Gen] = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] = fa.flatMap(f)
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

}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}
