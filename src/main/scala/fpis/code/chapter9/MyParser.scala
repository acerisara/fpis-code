package fpis.code.chapter9

import fpis.code.chapter9.MyParser.Parser

import scala.Function.tupled
import scala.language.implicitConversions
import scala.util.matching.Regex

class MyParser extends Parsers[Parser] {

  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
    p(Location(input)) match {
      case Success(a, _) => Right(a)
      case Failure(e, _) => Left(e)
    }

  override def or[A](p: Parser[A], p2: => Parser[A]): Parser[A] =
    location =>
      p(location) match {
        case Failure(_, false) => p2(location)
        case r                 => r
      }

  override def succeed[A](a: A): Parser[A] = _ => Success(a, 0)

  override def slice[A](p: Parser[A]): Parser[String] = (location: Location) =>
    p(location) match {
      case Success(_, c)     => Success(location.consumed(c), 0)
      case f @ Failure(_, _) => f
    }

  override def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] =
    (location: Location) =>
      p(location) match {
        case Success(a, c) =>
          f(a)(location.advanceBy(c))
            .addCommit(c != 0)
            .advanceSuccess(c)
        case f @ Failure(_, _) => f
      }

  override def label[A](msg: String)(p: Parser[A]): Parser[A] =
    location => p(location).mapError(_.label(msg))

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    location => p(location).mapError(_.push(location, msg))

  override def attempt[A](p: Parser[A]): Parser[A] = location =>
    p(location).uncommit

  override implicit def string(s: String): Parser[String] =
    (location: Location) => {
      val commonPrefix = s
        .zip(location.toParse)
        .takeWhile(tupled(_ == _))
        .length

      if (s.length == commonPrefix) {
        Success(s, s.length)
      } else
        Failure(location.toError(s"expected: ($s)"), commonPrefix != 0)
    }

  override implicit def regex(r: Regex): Parser[String] =
    (location: Location) =>
      r.findPrefixOf(location.toParse) match {
        case None =>
          Failure(
            location.toError(s"expected string matching: ($r)"),
            isCommitted = false
          )
        case Some(m) => Success(m, m.length)
      }
}

trait Result[+A] {

  def mapError(f: ParseError => ParseError): Result[A] = this match {
    case Failure(e, c) => Failure(f(e), c)
    case _             => this
  }

  def uncommit: Result[A] = this match {
    case Failure(e, true) => Failure(e, isCommitted = false)
    case _                => this
  }

  def addCommit(isCommitted: Boolean): Result[A] = this match {
    case Failure(e, c) => Failure(e, c || isCommitted)
    case _             => this
  }

  def advanceSuccess(n: Int): Result[A] = this match {
    case Success(a, m) => Success(a, n + m)
    case _             => this
  }

}

case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
case class Failure(get: ParseError, isCommitted: Boolean)
    extends Result[Nothing]

object MyParser {

  type Parser[+A] = Location => Result[A]

}
