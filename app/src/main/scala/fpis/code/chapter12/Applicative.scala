package fpis.code.chapter12

import fpis.code.chapter11.Functor

trait Applicative[F[_]] extends Functor[F] {
  // Minimal sets of combinators:
  // 1. map2, unit
  // 2. apply, unit

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  def unit[A](a: => A): F[A]

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((f, a) => f(a))

  def map2A[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(apply(unit(f.curried))(fa))(fb)

  def mapA[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)

  // derived combinators
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def sequence[A](lfa: List[F[A]]): F[List[A]] = traverse(lfa)(fa => fa)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def filterM[A](la: List[A])(f: A => F[Boolean]): F[List[A]] =
    la.foldLeft(unit(List.empty[A]))((fbs, a) =>
      map2(fbs, f(a))((as, include) => if (include) as :+ a else as)
    )

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(
      f: (A, B, C, D) => E
  ): F[E] = apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

}
