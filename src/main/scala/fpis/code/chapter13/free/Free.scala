package fpis.code.chapter13.free

import fpis.code.chapter11.Monad
import fpis.code.chapter11.Monad.parMonad
import fpis.code.chapter7.Par
import fpis.code.chapter7.Par.Par

import java.util.concurrent.ExecutorService
import scala.annotation.tailrec

sealed trait Free[F[_], A] {

  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)
  def map[B](f: A => B): Free[F, B] = flatMap(f andThen (Return(_)))

}

case class Return[F[_], A](a: A) extends Free[F, A]
case class Suspend[F[_], A](resume: F[A]) extends Free[F, A]
case class FlatMap[F[_], A, B](sub: Free[F, A], k: A => Free[F, B])
    extends Free[F, B]

object Free {

  type IO[A] = Free[Par, A]
  def IO[A](a: => A): IO[A] = Suspend { Par.lazyUnit(a) }

  trait Translate[F[_], G[_]] { def apply[A](f: F[A]): G[A] }
  type ~>[F[_], G[_]] = Translate[F, G]

  def freeMonad[F[_]]: Monad[({ type f[a] = Free[F, a] })#f] = new Monad[
    ({
      type f[a] = Free[F, a]
    })#f
  ] {
    override def unit[A](a: => A): Free[F, A] = Return(a)
    override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] =
      fa flatMap f
  }

  @tailrec
  def runTrampoline[A](free: Free[Function0, A]): A = free match {
    case Return(a)  => a
    case Suspend(r) => r()
    case FlatMap(x, f) =>
      x match {
        case Return(a)     => runTrampoline(f(a))
        case Suspend(r)    => runTrampoline(f(r()))
        case FlatMap(y, g) => runTrampoline(y.flatMap(a => g(a).flatMap(f)))
      }
  }

  @tailrec
  def step[F[_], A](free: Free[F, A]): Free[F, A] = free match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f)     => step(f(x))
    case _                         => free
  }

  def run[F[_], A](free: Free[F, A])(implicit F: Monad[F]): F[A] =
    step(free) match {
      case Return(a)  => F.unit(a)
      case Suspend(r) => r
      case FlatMap(x, f) =>
        x match {
          case Suspend(r) => F.flatMap(r)(a => run(f(a)))
          case _ => sys.error("Impossible: `step` eliminates these cases")
        }
    }

  def runFree[F[_], G[_], A](
      free: Free[F, A]
  )(t: F ~> G)(implicit G: Monad[G]): G[A] =
    step(free) match {
      case Return(a)              => G.unit(a)
      case Suspend(r)             => t(r)
      case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
      case _ => sys.error("Impossible: `step` eliminates these cases")
    }

  def forever[F[_], A, B](free: Free[F, A]): Free[F, B] = {
    lazy val t: Free[F, B] = forever(free)
    free flatMap (_ => t)
  }

  def unsafePerformIO[A](a: IO[A])(pool: ExecutorService): A =
    Par.run(pool)(run(a)(parMonad)).get()

}
