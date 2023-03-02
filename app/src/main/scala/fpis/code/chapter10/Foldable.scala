package fpis.code.chapter10

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
  def toList[A](fa: F[A]): List[A]
}

object Foldable {

  val foldableList: Foldable[List] = new Foldable[List] {
    override def foldRight[A, B](as: List[A])(acc: B)(f: (A, B) => B): B =
      as.foldRight(acc)(f)

    override def foldLeft[A, B](as: List[A])(acc: B)(f: (B, A) => B): B =
      as.foldLeft(acc)(f)

    override def foldMap[A, B](as: List[A])(f: A => B)(m: Monoid[B]): B =
      as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

    override def toList[A](as: List[A]): List[A] = as
  }

  val foldableIndexedSeq: Foldable[IndexedSeq] = new Foldable[IndexedSeq] {
    override def foldRight[A, B](as: IndexedSeq[A])(acc: B)(f: (A, B) => B): B =
      as.foldRight(acc)(f)

    override def foldLeft[A, B](as: IndexedSeq[A])(acc: B)(f: (B, A) => B): B =
      as.foldLeft(acc)(f)

    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(m: Monoid[B]): B =
      as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

    override def toList[A](as: IndexedSeq[A]): List[A] = as.toList
  }

  val foldableLazyList: Foldable[LazyList] = new Foldable[LazyList] {
    override def foldRight[A, B](as: LazyList[A])(acc: B)(f: (A, B) => B): B =
      as.foldRight(acc)(f)

    override def foldLeft[A, B](as: LazyList[A])(acc: B)(f: (B, A) => B): B =
      as.foldLeft(acc)(f)

    override def foldMap[A, B](as: LazyList[A])(f: A => B)(m: Monoid[B]): B =
      as.map(f).foldLeft(m.zero)((b, a) => m.op(b, a))

    override def toList[A](as: LazyList[A]): List[A] = as.toList
  }

}
