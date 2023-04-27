package fpis.code.chapter13.free

import fpis.code.chapter11.Monad
import fpis.code.chapter13.free.Free.~>
import fpis.code.chapter7.Par
import fpis.code.chapter7.Par.Par

import scala.io.StdIn.readLine

sealed trait Console[A] {

  def toPar: Par[A]
  def toThunk: () => A

}

case object ReadLine extends Console[Option[String]] {
  def toPar: Par[Option[String]] = Par.lazyUnit(run)
  def toThunk: () => Option[String] = () => run

  def run: Option[String] =
    try Some(readLine())
    catch { case _: Exception => None }
}

case class PrintLine(line: String) extends Console[Unit] {
  def toPar: Par[Unit] = Par.lazyUnit(println(line))
  def toThunk: () => Unit = () => println(line)
}

object Console {

  type ConsoleIO[A] = Free[Console, A]

  def readLine: ConsoleIO[Option[String]] = Suspend(ReadLine)
  def printLine(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))

  val consoleToFunction0: Console ~> Function0 = new (Console ~> Function0) {
    def apply[A](a: Console[A]): () => A = a.toThunk
  }

  val consoleToPar: Console ~> Par = new (Console ~> Par) {
    def apply[A](a: Console[A]): Par[A] = a.toPar
  }

  def runConsolePar[A](a: Free[Console, A]): Par[A] =
    Free.runFree[Console, Par, A](a)(consoleToPar)

  def runConsoleFunction0[A](a: Free[Console, A]): () => A =
    Free.runFree[Console, Function0, A](a)(consoleToFunction0)

  implicit val function0Monad: Monad[Function0] = new Monad[Function0] {
    def unit[A](a: => A): () => A = () => a

    override def flatMap[A, B](a: () => A)(f: A => () => B): () => B = () =>
      f(a())()
  }

  implicit val parMonad: Monad[Par] = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
      Par.fork {
        Par.flatMap(a)(f)
      }
  }

}
