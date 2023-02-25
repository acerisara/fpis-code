package fpis.code.chapter10

import fpis.code.chapter7.Par
import fpis.code.chapter7.Par.{Par, asyncF, lazyUnit}
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
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a1.andThen(a2)
    override def zero: A => A = a => a
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.map(f).foldLeft(m.zero)(m.op)

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.isEmpty) m.zero
    else if (v.length <= 1) f(v.head)
    else {
      val (subL, subR) = v.splitAt(v.length / 2)
      m.op(foldMapV(subL, m)(f), foldMapV(subR, m)(f))
    }
  }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(pa1: Par[A], pa2: Par[A]): Par[A] = Par.map2(pa1, pa2) {
      (a1, a2) => m.op(a1, a2)
    }
    override def zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    foldMapV(v, par(m))(a => lazyUnit(f(a)))

  def isOrdered(v: IndexedSeq[Int]): Boolean = {
    case class Interval(ordered: Boolean, min: Int, max: Int)

    val orderedMonoid = new Monoid[Option[Interval]] {
      override def op(
          oa1: Option[Interval],
          oa2: Option[Interval]
      ): Option[Interval] = (oa1, oa2) match {
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

    foldMapV(v, orderedMonoid)(i => Some(Interval(ordered = true, i, i)))
      .forall(_.ordered)
  }

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = forAll(for {
    x <- gen
    y <- gen
    z <- gen
  } yield (x, y, z))(p => {
    // Associativity
    m.op(p._1, m.op(p._2, p._3)) == m.op(m.op(p._1, p._2), p._3) &&
      // Identity
      m.op(p._1, m.zero) == p._1 &&
      m.op(p._2, m.zero) == p._2 &&
      m.op(p._3, m.zero) == p._3
  })

}
