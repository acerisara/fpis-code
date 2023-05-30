package fpis.code.chapter15.process1

import fpis.code.chapter11.Monad
import fpis.code.chapter13.io.IO
import fpis.code.chapter15.process1.Process.lift

import java.io.PrintWriter

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

  def |>[O2](p2: Process[O, O2]): Process[I, O2] = {
    p2 match {
      case Halt()     => Halt()
      case Emit(h, t) => Emit(h, this |> t)
      case Await(recv) =>
        this match {
          case Emit(h, t)   => t |> recv(Some(h))
          case Halt()       => Halt() |> recv(None)
          case Await(recv2) => Await((i: Option[I]) => recv2(i) |> p2)
        }
    }
  }

  def map[O2](f: O => O2): Process[I, O2] = this |> lift(f)

  def ++(p: => Process[I, O]): Process[I, O] = this match {
    case Halt()      => p
    case Emit(h, t)  => Emit(h, t ++ p)
    case Await(recv) => Await(recv andThen (_ ++ p))
  }

  def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] = this match {
    case Halt()      => Halt()
    case Emit(h, t)  => f(h) ++ t.flatMap(f)
    case Await(recv) => Await(recv andThen (_ flatMap f))
  }

  def zipWithIndex(): Process[I, (Int, O)] = {
    def go(acc: Int, p: Process[I, O]): Process[I, (Int, O)] = p match {
      case Halt()     => Halt()
      case Emit(h, t) => Emit((acc, h), go(acc + 1, t))
      case Await(recv) =>
        Await {
          case Some(i) => go(acc, recv(Some(i)))
          case None    => go(acc, Halt())
        }
    }

    go(0, this)
  }

}

case class Halt[I, O]() extends Process[I, O]
case class Await[I, O](recv: Option[I] => Process[I, O]) extends Process[I, O]
case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]())
    extends Process[I, O]

object Process {

  def liftOne[I, O](f: I => O): Process[I, O] =
    Await {
      case Some(i) => Emit(f(i))
      case None    => Halt()
    }

  def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

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

  def monad[I]: Monad[({ type f[x] = Process[I, x] })#f] = new Monad[
    ({
      type f[x] = Process[I, x]
    })#f
  ] {
    override def unit[O](o: => O): Process[I, O] = Emit(o)

    override def flatMap[O, O2](p: Process[I, O])(
        f: O => Process[I, O2]
    ): Process[I, O2] = p flatMap f
  }

  def exists[I](f: I => Boolean): Process[I, Boolean] =
    Await[I, Boolean] {
      case Some(i) if f(i) => Emit(true)
      case Some(_)         => exists(f)
      case _               => Emit(false)
    }

  def processFile[A, B](
      file: java.io.File,
      process: Process[String, A],
      acc: B
  )(g: (B, A) => B): IO[B] = IO {
    @annotation.tailrec
    def go(lines: Iterator[String], process: Process[String, A], acc: B): B =
      process match {
        case Halt() => acc
        case Await(recv) =>
          val next =
            if (lines.hasNext) recv(Some(lines.next()))
            else recv(None)
          go(lines, next, acc)
        case Emit(h, t) => go(lines, t, g(acc, h))
      }

    val reader = io.Source.fromFile(file)
    try go(reader.getLines(), process, acc)
    finally reader.close
  }

  def transformFile[A](
      input: java.io.File,
      output: java.io.File,
      process: Process[String, A]
  ): IO[Unit] = IO {
    @annotation.tailrec
    def go(
        lines: Iterator[String],
        process: Process[String, A],
        writer: PrintWriter
    ): Unit =
      process match {
        case Halt() => Halt()
        case Await(recv) =>
          val next =
            if (lines.hasNext) recv(Some(lines.next()))
            else recv(None)
          go(lines, next, writer)
        case Emit(h, t) =>
          writer.println(h)
          go(lines, t, writer)
      }

    val reader = io.Source.fromFile(input)
    val writer = new PrintWriter(output)

    try go(reader.getLines(), process, writer)
    finally {
      reader.close()
      writer.close()
    }
  }

  def toCelsiusTransformer(
      input: java.io.File,
      output: java.io.File
  ): IO[Unit] = {
    def toCelsius(fahrenheit: Double): Double =
      (5.0 / 9.0) * (fahrenheit - 32.0)

    // TODO: Skip comments (#) and empty lines
    val p = lift[String, Double](_.toDouble) |> lift[Double, Double](toCelsius)
    transformFile(input, output, p)
  }

}
