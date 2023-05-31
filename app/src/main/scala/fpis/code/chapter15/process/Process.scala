package fpis.code.chapter15.process

import fpis.code.chapter11.Monad
import fpis.code.chapter13.free.Free.{IO, unsafePerformIO}
import fpis.code.chapter15.process.Process._

import scala.annotation.tailrec

trait Process[F[_], O] {

  def onHalt(f: Throwable => Process[F, O]): Process[F, O] = this match {
    case Halt(e)          => Try(f(e))
    case Emit(h, t)       => Emit(h, t.onHalt(f))
    case Await(req, recv) => Await(req, recv andThen (_.onHalt(f)))
  }

  def ++(p: => Process[F, O]): Process[F, O] =
    this.onHalt {
      case End => p
      case err => Halt(err)
    }

  def flatMap[O2](f: O => Process[F, O2]): Process[F, O2] =
    this match {
      case Halt(err)  => Halt(err)
      case Emit(o, t) => Try(f(o)) ++ t.flatMap(f)
      case Await(req, recv) =>
        Await(req, recv andThen (_ flatMap f))
    }

  def runLog(implicit F: MonadCatch[F]): F[IndexedSeq[O]] = {
    def go(p: Process[F, O], acc: IndexedSeq[O]): F[IndexedSeq[O]] =
      p match {
        case Emit(h, t) => go(t, acc :+ h)
        case Halt(End)  => F.unit(acc)
        case Halt(err)  => F.fail(err)
        case Await(req, recv) =>
          F.flatMap(F.attempt(req)) { e => go(Try(recv(e)), acc) }
      }

    go(this, IndexedSeq())
  }

}

object Process {

  def await[F[_], A, O](req: F[A])(
      recv: Either[Throwable, A] => Process[F, O]
  ): Process[F, O] = Await(req, recv)

  def runLog[O](src: Process[IO, O]): IO[IndexedSeq[O]] = IO {
    val es = java.util.concurrent.Executors.newFixedThreadPool(4)

    @tailrec
    def go(p: Process[IO, O], acc: IndexedSeq[O]): IndexedSeq[O] =
      p match {
        case Emit(h, t) => go(t, acc :+ h)
        case Halt(End)  => acc
        case Halt(err)  => throw err
        case Await(req, recv) =>
          val next =
            try recv(Right(unsafePerformIO(req)(es)))
            catch {
              case err: Throwable => recv(Left(err))
            }
          go(next, acc)
      }

    try go(src, IndexedSeq())
    finally es.shutdown()
  }

  import java.io.{BufferedReader, FileReader}

  def getLines(fileName: String): Process[IO, String] =
    await(IO(new BufferedReader(new FileReader(fileName)))) {
      case Right(b) =>
        lazy val next: Process[IO, String] = await(IO(b.readLine)) {
          case Left(e) => await(IO(b.close()))(_ => Halt(e))
          case Right(line) =>
            if (line eq null) Halt(End)
            else Emit(line, next)
        }
        next
      case Left(e) => Halt(e)
    }

  def Try[F[_], O](p: => Process[F, O]): Process[F, O] =
    try p
    catch {
      case e: Throwable => Halt(e)
    }

  case class Await[F[_], A, O](
      req: F[A],
      recv: Either[Throwable, A] => Process[F, O]
  ) extends Process[F, O]

  case class Emit[F[_], O](head: O, tail: Process[F, O]) extends Process[F, O]
  case class Halt[F[_], O](err: Throwable) extends Process[F, O]

  case object End extends Exception
  case object Kill extends Exception

  trait MonadCatch[F[_]] extends Monad[F] {
    def attempt[A](a: F[A]): F[Either[Throwable, A]]
    def fail[A](t: Throwable): F[A]
  }

}
