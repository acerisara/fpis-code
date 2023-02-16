package fpis.code.chapter9

import fpis.code.chapter8.Prop.forAll
import fpis.code.chapter8.{Gen, Prop}

import scala.language.implicitConversions
import scala.util.matching.Regex

case class Location(input: String, offset: Int = 0) {

  def toError(msg: String): ParseError = ParseError(List((this, msg)))

  def toParse: String = input.substring(offset)

  def consumed(charConsumed: Int): String =
    input.substring(offset, charConsumed)

  def advanceBy(n: Int): Location = copy(offset = offset + n)

}

case class ParseError(stack: List[(Location, String)]) {

  def push(location: Location, msg: String): ParseError =
    copy(stack = (location, msg) :: stack)

  def label(s: String): ParseError =
    ParseError(latestLocation.map((_, s)).toList)

  def latestLocation: Option[Location] = latest.map(_._1)

  def latest: Option[(Location, String)] = stack.lastOption

}

trait Parsers[Parser[+_]] { self =>

  // Primitives
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def or[A](p: Parser[A], p2: => Parser[A]): Parser[A]

  def succeed[A](a: A): Parser[A]

  def slice[A](p: Parser[A]): Parser[String]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  implicit def string(s: String): Parser[String]

  implicit def regex(r: Regex): Parser[String]

  // Combinators
  def char(c: Char): Parser[String] = string(c.toString)

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n > 0) map2(p, listOfN(n - 1, p))(_ :: _) else succeed(List())

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _).or(succeed(List()))

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    flatMap(p)(a => succeed(f(a)))

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

  def opt[A](p: Parser[A]): Parser[Option[A]] =
    p.map(Some(_)) or succeed(None)

  // Accessories
  def digit: Parser[String] = """\d""".r

  def thatManyChars(c: Char): Parser[String] =
    flatMap(digit)(n => listOfN(n.toInt, char(c)).toString)

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit
      f: A => Parser[String]
  ): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def many: Parser[String] = self.many(p).map(l => l.foldLeft("")(_ + _))
    def manyL: Parser[List[A]] = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def opt: Parser[Option[A]] = self.opt(p)

    def >>[B](p2: Parser[B]): Parser[B] =
      self.map(self.product(p, p2))(a => a._2)

    def <<[B](p2: Parser[B]): Parser[A] =
      self.map(self.product(p, p2))(a => a._1)

    def ++(p2: Parser[List[A]]): Parser[List[A]] = {
      self.map(self.product(p, p2)) { case (a, as) =>
        a +: as
      }
    }
  }

  object Laws {
    private def equal[A](p: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def succeedLaw[A]()(in: Gen[(String, A)]): Prop =
      forAll(in) { case (s, a) =>
        run(succeed(a))(s) == Right(a)
      }
  }

}
