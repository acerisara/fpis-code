package fpis.code.chapter10

import fpis.code.chapter7.Par
import fpis.code.chapter7.Par.{Par, lazyUnit}
import fpis.code.chapter8.Prop.forAll
import fpis.code.chapter8.{Gen, Prop}

trait Monoid[A] {

  def op(a1: A, a2: A): A

  def zero: A

}

object Monoid {

  val stringMonoid: Monoid[String] = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2
    override def zero: String = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    override def zero: List[A] = Nil
  }

  val intAdditionMonoid: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }

  val intMultiplicationMonoid: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }

  val booleanOrMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = false
  }

  val booleanAndMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    // We have the option of going either a1.orElse(a2) or a2.orElse(a1)
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    // We have the option of going either a1(a2) or a2(a1)
    override def op(a1: A => A, a2: A => A): A => A = a1.andThen(a2)
    override def zero: A => A = a => a
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def op(a1: A, a2: A): A = m.op(a2, a1)
    override def zero: A = m.zero
  }

  def functionMonoid[A, B](b: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      override def op(a1: A => B, a2: A => B): A => B = a => b.op(a1(a), a2(a))
      override def zero: A => B = _ => b.zero
    }

  def parMonoid[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2) {
      (a1, a2) => m.op(a1, a2)
    }

    override def zero: Par[A] = Par.unit(m.zero)
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (as.isEmpty) m.zero
    else if (as.length <= 1) f(as.head)
    else {
      val (subL, subR) = as.splitAt(as.length / 2)
      m.op(foldMapV(subL, m)(f), foldMapV(subR, m)(f))
    }
  }

  def parFoldMap[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    foldMapV(as, parMonoid(m))(a => lazyUnit(f(a)))

  def isOrdered(ints: IndexedSeq[Int]): Boolean = {
    case class Interval(ordered: Boolean, min: Int, max: Int)

    val orderedMonoid = new Monoid[Option[Interval]] {
      override def op(
          a1: Option[Interval],
          a2: Option[Interval]
      ): Option[Interval] = (a1, a2) match {
        case (Some(a1), Some(a2)) =>
          Some(
            Interval(
              a1.ordered && a2.ordered && a1.max <= a2.min,
              a1.min,
              a2.max
            )
          )
        case (x, None) => x
        case (None, x) => x
      }

      override def zero: Option[Interval] = None
    }

    foldMapV(ints, orderedMonoid)(i => Some(Interval(ordered = true, i, i)))
      .forall(_.ordered)
  }

  def productMonoid[A, B](m1: Monoid[A], m2: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      override def op(a1: (A, B), a2: (A, B)): (A, B) =
        (m1.op(a1._1, a2._1), m2.op(a1._2, a2._2))

      override def zero: (A, B) = (m1.zero, m2.zero)
    }

  def mapMergeMonoid[K, V](m: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      override def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] =
        (a1.keySet ++ a2.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(k, m.op(a1.getOrElse(k, m.zero), a2.getOrElse(k, m.zero)))
        }

      override def zero: Map[K, V] = Map[K, V]()
    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, listMonoid[(A, Int)])(a => List((a, as.count(_ == a)))).toMap

  def bagM[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAdditionMonoid))(a => Map(a -> 1))

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    forAll(for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z))(p => {
      // Associativity
      m.op(p._1, m.op(p._2, p._3)) == m.op(m.op(p._1, p._2), p._3)
    }) && // Identity
      forAll(gen)((a: A) => m.op(a, m.zero) == a && m.op(m.zero, a) == a)

}
