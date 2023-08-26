package fpis.code.chapter10

import fpis.code.chapter10.Monoid.{dual, endoMonoid}
import fpis.code.chapter3.Tree

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = {
    // A => (B => B)
    val fCurried = f.curried
    // Monoid to fold B => B via function composition
    val m = endoMonoid[B]
    foldMap(as)(fCurried)(m)(z)
  }

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = {
    // We simply flip the arguments of f here and curry like in foldRight
    val g = (a: A, b: B) => f(b, a)
    val gCurried = g.curried
    // But we also need to flip the monoid
    val m = dual(endoMonoid[B])
    foldMap(as)(gCurried)(m)(z)
  }

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](fa: F[A]): List[A] =
    foldLeft(fa)(List.empty[A])((b, a) => a :: b)
}

object Foldable {

  val foldableList: Foldable[List] = new Foldable[List] {
    override def foldMap[A, B](as: List[A])(f: A => B)(m: Monoid[B]): B =
      as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
  }

  val foldableIndexedSeq: Foldable[IndexedSeq] = new Foldable[IndexedSeq] {
    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(m: Monoid[B]): B =
      as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
  }

  val foldableLazyList: Foldable[LazyList] = new Foldable[LazyList] {
    override def foldMap[A, B](as: LazyList[A])(f: A => B)(m: Monoid[B]): B =
      as.map(f).foldLeft(m.zero)((b, a) => m.op(b, a))
  }

  val foldableTree: Foldable[Tree] = new Foldable[Tree] {
    override def foldMap[A, B](as: Tree[A])(f: A => B)(m: Monoid[B]): B =
      Tree.fold(as)(f)((b, a) => m.op(b, a))
  }

  val foldableOption: Foldable[Option] = new Foldable[Option] {
    override def foldMap[A, B](as: Option[A])(f: A => B)(m: Monoid[B]): B =
      as match {
        case None    => m.zero
        case Some(a) => f(a)
      }
  }

}
