package fpis.code.chapter9

import fpis.code.chapter9.MyParser.Parser

import scala.language.implicitConversions
import scala.util.matching.Regex

class MyParser extends Parsers[Parser] {

  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
    p(Location(input)) match {
      case Success(a, _) => Right(a)
      case Failure(e)    => Left(e)
    }

  override def or[A](p: Parser[A], p2: => Parser[A]): Parser[A] = ???

  override def succeed[A](a: A): Parser[A] = _ => Success(a, 0)

  override def slice[A](p: Parser[A]): Parser[String] = (location: Location) =>
    p(location) match {
      case Success(_, c)  => Success(location.consumed(c), 0)
      case f @ Failure(_) => f
    }

  override def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = ???

  override def label[A](msg: String)(p: Parser[A]): Parser[A] = ???

  override def errorLocation(e: ParseError): Location = ???

  override def errorMessage(e: ParseError): String = ???

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] = ???

  override def attempt[A](p: Parser[A]): Parser[A] = ???

  override implicit def string(s: String): Parser[String] =
    (location: Location) =>
      if (location.toParse.startsWith(s))
        Success(s, s.length)
      else
        Failure(location.toError("Expected: " + s))

  override implicit def regex(r: Regex): Parser[String] =
    (location: Location) =>
      r.findPrefixOf(location.input) match {
        case None => Failure(location.toError("Expected string matching: " + r))
        case Some(m) => Success(m, m.length)
      }
}

trait Result[+A]
case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
case class Failure(get: ParseError) extends Result[Nothing]

object MyParser {

  type Parser[+A] = Location => Result[A]

}
