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

  def asFinalizer: Process[F, O] = this match {
    case Emit(h, t) => Emit(h, t.asFinalizer)
    case Halt(e)    => Halt(e)
    case Await(req, recv) =>
      await(req) {
        case Left(Kill) => this.asFinalizer
        case x          => recv(x)
      }
  }

  def onComplete(p: => Process[F, O]): Process[F, O] =
    this.onHalt {
      case End => p.asFinalizer
      case err => p.asFinalizer ++ Halt(err)
    }

  def drain[O2]: Process[F, O2] = this match {
    case Halt(e)          => Halt(e)
    case Emit(h, t)       => t.drain
    case Await(req, recv) => Await(req, recv andThen (_.drain))
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

  def eval[F[_], A](a: F[A]): Process[F, A] =
    await[F, A, A](a) {
      case Left(err) => Halt(err)
      case Right(a)  => Emit(a, Halt(End))
    }

  def eval_[F[_], A, B](a: F[A]): Process[F, B] =
    eval[F, A](a).drain[B]

  def resource[R, O](acquire: IO[R])(use: R => Process[IO, O])(
      release: R => Process[IO, O]
  ): Process[IO, O] =
    eval[IO, R](acquire).flatMap(r => use(r).onComplete(release(r)))

  def lines(filename: String): Process[IO, String] =
    resource {
      IO(io.Source.fromFile(filename))
    } { src =>
      lazy val iter = src.getLines()
      def step: Option[String] = if (iter.hasNext) Some(iter.next()) else None

      lazy val lines: Process[IO, String] = eval(IO(step)).flatMap {
        case None       => Halt(End)
        case Some(line) => Emit(line, lines)
      }

      lines
    } { src => eval_ { IO(src.close) } }

  def join[F[_], O](p: Process[F, Process[F, O]]): Process[F, O] =
    p.flatMap(a => a)

}
