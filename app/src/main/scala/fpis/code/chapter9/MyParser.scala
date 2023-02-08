package fpis.code.chapter9

import fpis.code.chapter9.MyParser.Parser

import scala.language.implicitConversions
import scala.util.matching.Regex

class MyParser extends Parsers[Parser] {

  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
    p(input)

  override def or[A](p: Parser[A], p2: => Parser[A]): Parser[A] = ???

  override def succeed[A](a: A): Parser[A] = ???

  override def slice[A](p: Parser[A]): Parser[String] = ???

  override def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = ???

  override def label[A](msg: String)(p: Parser[A]): Parser[A] = ???

  override def errorLocation(e: ParseError): Location = ???

  override def errorMessage(e: ParseError): String = ???

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] = ???

  override def attempt[A](p: Parser[A]): Parser[A] = ???

  override implicit def string(s: String): Parser[String] =
    (input: String) =>
      if (input.startsWith(s))
        Right(s)
      else
        Left(Location(input).toError("Expected: " + s))

  override implicit def regex(r: Regex): Parser[String] = ???
}

object MyParser {

  type Parser[+A] = String => Either[ParseError, A]

}
