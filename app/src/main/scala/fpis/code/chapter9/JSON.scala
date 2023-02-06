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

    def jString(): Parser[JString] = {
      val quote = char('"')
      val chars = """\w""".r.many

      // Simplified version, doesn't support control characters
      (quote ** chars ** quote).map { case ((_, string), _) =>
        JString(string)
      }
    }

    def jNull(): Parser[JSON] = {
      string("null").map(_ => JNull)
    }

    def jBool(): Parser[JBool] = {
      (string("true") | string("false")).map(s => JBool("true" == s))
    }

    def jValue(): Parser[JSON] = {
      val whitespace = whiteSpace()

      (whitespace ** (jString() | jNumber() | jBool() | jNull() | jArray() | jObject()) ** whitespace)
        .map { case ((_, value), _) => value }
    }

    def jArray(): Parser[JArray] = {
      val open = char('[')
      val close = char(']')
      val comma = char(',')
      val value = jValue()

      val values = (value ** (comma ** value).manyL.map(_.map(_._2))).map {
        case (value, values) =>
          (value +: values).toIndexedSeq
      }

      val body = (whiteSpace().map(_ => Vector()) | values).map(v => JArray(v))

      (open ** body ** close).map { case ((_, values), _) => values }
    }

    def jObject(): Parser[JObject] = {
      val open = char('{')
      val close = char('}')
      val comma = char(',')
      val colon = char(':')
      val whitespace = whiteSpace()
      val string = jString()
      val value = jValue()

      val field = (whitespace ** string ** whitespace ** colon ** value).map {
        case ((((_, name), _), _), value) =>
          name.get -> value
      }

      val fields = (field ** (comma ** field).manyL.map(_.map(_._2))).map {
        case (value, values) =>
          value +: values
      }

      val body = (whitespace.map(_ => List()) | fields).map { v =>
        JObject(v.toMap)
      }

      (open ** body ** close).map { case ((_, fields), _) => fields }
    }

    jObject()
  }
}
