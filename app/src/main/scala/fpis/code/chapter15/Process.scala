package fpis.code.chapter15

sealed trait Process[I, O] {

  def apply(i: LazyList[I]): LazyList[O] = this match {
    case Halt() => LazyList()
    case Await(recv) =>
      i match {
        case h #:: t => recv(Some(h))(t)
        case xs      => recv(None)(xs)
      }
    case Emit(h, t) => h #:: t(i)
  }

  def repeat: Process[I, O] = {
    def go(p: Process[I, O]): Process[I, O] = p match {
      case Halt() => go(this)
      case Await(recv) =>
        Await {
          case None => recv(None)
          case i    => go(recv(i))
        }
      case Emit(h, t) => Emit(h, go(t))
    }

    go(this)
  }

}

case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]())
    extends Process[I, O]
case class Await[I, O](recv: Option[I] => Process[I, O]) extends Process[I, O]
case class Halt[I, O]() extends Process[I, O]

object Process {

  def liftOne[I, O](f: I => O): Process[I, O] =
    Await {
      case Some(i) => Emit(f(i))
      case None    => Halt()
    }

  def filter[I](p: I => Boolean): Process[I, I] =
    Await[I, I] {
      case Some(i) if p(i) => Emit(i)
      case _               => Halt()
    }.repeat

  def sum: Process[Double, Double] = {
    def go(acc: Double): Process[Double, Double] =
      Await {
        case Some(d) => Emit(d + acc, go(d + acc))
        case _       => Halt()
      }

    go(0.0)
  }

  def take[I](n: Int): Process[I, I] = {
    def go(n: Int): Process[I, I] =
      Await[I, I] {
        case Some(i) if n > 0 => Emit(i, go(n - 1))
        case _                => Halt()
      }

    go(n)
  }

  def drop[I](n: Int): Process[I, I] = {
    def go(n: Int): Process[I, I] =
      Await[I, I] {
        case Some(_) if n > 0 => go(n - 1)
        case Some(i)          => Emit(i, go(n))
        case _                => Halt()
      }

    go(n)
  }

  def takeWhile[I](f: I => Boolean): Process[I, I] =
    Await {
      case Some(i) if f(i) => Emit(i, takeWhile(f))
      case _               => Halt()
    }

  def dropWhile[I](f: I => Boolean): Process[I, I] =
    Await {
      case Some(i) if f(i) => dropWhile(f)
      case Some(i)         => Emit(i, dropWhile(f))
      case _               => Halt()
    }

  def count[I]: Process[I, Int] = {
    def go(count: Int): Process[I, Int] =
      Await[I, Int] {
        case Some(_) => Emit(count, go(count + 1))
        case _       => Halt()
      }

    go(1)
  }

  def mean: Process[Double, Double] = {
    def go(sum: Double, n: Int): Process[Double, Double] = {
      Await[Double, Double] {
        case Some(i) => Emit((sum + i) / (n + 1), go(sum + i, n + 1))
        case _       => Halt()
      }
    }

    go(0, 0)
  }

  def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] =
    Await[I, O] {
      case Some(i) =>
        f(i, z) match {
          case (o, s2) => Emit(o, loop(s2)(f))
        }
      case _ => Halt()
    }

  def sumL: Process[Double, Double] = loop(0.0)((i, s) => (i + s, i + s))

  def countL[I]: Process[I, Int] = loop(1)((_, s) => (s, s + 1))

}
