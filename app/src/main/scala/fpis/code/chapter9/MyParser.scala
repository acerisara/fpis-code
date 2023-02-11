package fpis.code.chapter9

import fpis.code.chapter9.MyParser.Parser

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

  override def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = ???

  override def label[A](msg: String)(p: Parser[A]): Parser[A] =
    location => p(location).mapError(_.label(msg))

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    location => p(location).mapError(_.push(location, msg))

  override def attempt[A](p: Parser[A]): Parser[A] = location =>
    p(location).uncommit

  override implicit def string(s: String): Parser[String] =
    (location: Location) =>
      if (location.toParse.startsWith(s))
        Success(s, s.length)
      else
        Failure(location.toError("Expected: " + s), isCommitted = true)

  override implicit def regex(r: Regex): Parser[String] =
    (location: Location) =>
      r.findPrefixOf(location.input) match {
        case None =>
          Failure(
            location.toError("Expected string matching: " + r),
            isCommitted = true
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

}

case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
case class Failure(get: ParseError, isCommitted: Boolean)
    extends Result[Nothing]

object MyParser {

  type Parser[+A] = Location => Result[A]

}
