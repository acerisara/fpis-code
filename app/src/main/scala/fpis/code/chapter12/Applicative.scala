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

  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(fa => fa)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

  def filterM[A](as: List[A])(f: A => F[Boolean]): F[List[A]] =
    as.foldLeft(unit(List.empty[A])) { (fas, a) =>
      map2(fas, f(a))((as, b) => if (b) as :+ a else as)
    }

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(
      f: (A, B, C, D) => E
  ): F[E] = apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  def sequenceMap[K, V](mfv: Map[K, F[V]]): F[Map[K, V]] =
    mfv.foldRight(unit(Map.empty[K, V])) { (kfv, fm) =>
      map2(kfv._2, fm)((v, m) => m + (kfv._1 -> v))
    }

}

object Applicative {

  def product[F[_], G[_]](
      F: Applicative[F],
      G: Applicative[G]
  ): Applicative[({ type f[x] = (F[x], G[x]) })#f] = new Applicative[
    ({
      type f[x] = (F[x], G[x])
    })#f
  ] {
    override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(
        f: (A, B) => C
    ): (F[C], G[C]) = (F.map2(fa._1, fb._1)(f), G.map2(fa._2, fb._2)(f))

    override def unit[A](a: => A): (F[A], G[A]) = (F.unit(a), G.unit(a))
  }

  def compose[F[_], G[_]](
      F: Applicative[F],
      G: Applicative[G]
  ): Applicative[({ type f[x] = F[G[x]] })#f] = new Applicative[
    ({
      type f[x] = F[G[x]]
    })#f
  ] {
    override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(
        f: (A, B) => C
    ): F[G[C]] = F.map2(fa, fb)(G.map2(_, _)(f))

    override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))
  }

}
