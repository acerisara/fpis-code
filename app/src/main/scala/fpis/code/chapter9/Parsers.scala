package fpis.code.chapter9

import fpis.code.chapter8.Prop.forAll
import fpis.code.chapter8.{Gen, Prop}

import scala.language.implicitConversions
import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+_]] { self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  def or[A](p: Parser[A], p2: => Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n > 0) map2(p, listOfN(n - 1, p))(_ :: _) else succeed(List())

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _).or(succeed(List()))

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = flatMap(p) { a =>
    succeed(f(a))
  }

  def succeed[A](a: A): Parser[A]

  def slice[A](p: Parser[A]): Parser[String]

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    for {
      a <- p
      b <- p2
    } yield (a, b)

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    for {
      a <- p
      b <- p2
    } yield f(a, b)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def thatManyChars(c: Char): Parser[String] =
    flatMap("""\d.*""".r)(n => listOfN(n.toInt, char(c)).toString)

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def regex(r: Regex): Parser[String]
  implicit def asStringParser[A](a: A)(implicit
      f: A => Parser[String]
  ): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def many(): Parser[List[A]] = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def slice(): Parser[String] = self.slice(p)
    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
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
