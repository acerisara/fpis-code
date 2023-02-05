package fpis.code.chapter9

trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[ParseError, Parser[+_]](
      P: Parsers[ParseError, Parser]
  ): Parser[JSON] = {
    import P._

    // WIP, grammar at https://www.json.org/json-en.html

    def whiteSpace(): Parser[String] = {
      val empty = ""
      val space = char(' ')
      val linefeed = char('\n')
      val carriageReturn = char('\r')
      val horizontalTab = char('\t')

      empty | (space | linefeed | carriageReturn | horizontalTab).many
    }

    def jNumber(): Parser[JNumber] = {
      val dot = char('.')
      val zero = char('0')
      val plus = char('+')
      val minus = char('-')
      val e = char('e') | char('E')

      val intDigits = digit1 *** digits.maybe
      val intNumber = zero | intDigits

      val integer = minus.maybe *** intNumber
      val fraction = dot *** digits
      val exponent = e *** (plus | minus).maybe *** digits

      (integer *** fraction.maybe *** exponent.maybe).map { n =>
        JNumber(n.toDouble)
      }
    }

    jNumber()
  }
}
