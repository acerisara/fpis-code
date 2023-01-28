package fpis.code.chapter9

import fpis.code.chapter8.Prop.forAll
import fpis.code.chapter8.{Gen, Prop}

import scala.language.implicitConversions

trait Parsers[ParseError, Parser[+_]] { self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))
  def or[A](p: Parser[A], p2: Parser[A]): Parser[A]
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]
  def many[A](p: Parser[A]): Parser[List[A]]
  def map[A, B](p: Parser[A])(f: A => B): Parser[B]
  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit
      f: A => Parser[String]
  ): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def many(): Parser[List[A]] = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
  }

  object Laws {
    def equal[A](p: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def succeedLaw[A]()(in: Gen[(String, A)]): Prop =
      forAll(in) { case (s, a) =>
        run(succeed(a))(s) == Right(a)
      }
  }

}
