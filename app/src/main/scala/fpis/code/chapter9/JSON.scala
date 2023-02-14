package fpis.code.chapter9

trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  val empty = ""

  def whitespaceParser[Parser[+_]](P: Parsers[Parser]): Parser[String] = {
    import P._

    val space = char(' ')
    val linefeed = char('\n')
    val carriageReturn = char('\r')
    val horizontalTab = char('\t')

    empty | (space | linefeed | carriageReturn | horizontalTab).many
  }

  def jStringParser[Parser[+_]](P: Parsers[Parser]): Parser[JString] = {
    import P._

    val quote = char('"')
    val chars = regex("\\w".r).many

    // Simplified version, doesn't support control characters
    (quote ** chars ** quote).map { case ((_, string), _) =>
      JString(string)
    }
  }

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    // WIP, grammar at https://www.json.org/json-en.html
    val ws = whitespaceParser(P)
    val jString = jStringParser(P)

    def jNumber: Parser[JNumber] = {
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

    def jNull: Parser[JSON] = {
      string("null").map(_ => JNull)
    }

    def jBool: Parser[JBool] = {
      (string("true") | string("false")).map(s => JBool("true" == s))
    }

    def jValue: Parser[JSON] = {
      val literal = jString | jNumber | jBool | jNull
      val value = literal | jArray | jObject

      (ws ** value ** ws)
        .map { case ((_, value), _) => value }
    }

    def jArray: Parser[JArray] = {
      val open = char('[')
      val close = char(']')
      val comma = char(',')
      val value = jValue

      val values = (value ** (comma ** value).manyL.map(_.map(_._2))).map {
        case (value, values) =>
          (value +: values).toIndexedSeq
      }

      val body = (ws.map(_ => Vector()) | values).map(v => JArray(v))

      (open ** body ** close).map { case ((_, values), _) => values }
    }

    def jObject: Parser[JObject] = {
      val open = char('{')
      val close = char('}')
      val comma = char(',')
      val colon = char(':')
      val value = jValue

      val field = (ws ** jString ** ws ** colon ** value).map {
        case ((((_, name), _), _), value) =>
          name.get -> value
      }

      val fields = (field ** (comma ** field).manyL.map(_.map(_._2))).map {
        case (value, values) =>
          value +: values
      }

      val body =
        (ws.map(_ => List()) | (ws ** fields).map(_._2)).map { v =>
          JObject(v.toMap)
        }

      (open ** body ** close).map { case ((_, fields), _) => fields }
    }

    jObject
  }
}
