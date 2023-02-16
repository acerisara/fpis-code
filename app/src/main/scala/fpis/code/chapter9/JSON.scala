package fpis.code.chapter9

trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def whitespaceParser[Parser[+_]](P: Parsers[Parser]): Parser[String] = {
    import P._

    val space = char(' ')
    val linefeed = char('\n')
    val carriageReturn = char('\r')
    val horizontalTab = char('\t')

    (space | linefeed | carriageReturn | horizontalTab).many
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

  def jNumberParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    // Simplified version, doesn't support e
    val number = regex("(-?)(0|([1-9][0-9]*))(\\.[0-9]+)?".r)
    number.map(n => JNumber(n.toDouble))
  }

  def jNullParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    string("null").map(_ => JNull)
  }

  def jBoolParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    (string("true") | string("false")).map(s => JBool("true" == s))
  }

  def jValueParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    val jString = jStringParser(P)
    val jNumber = jNumberParser(P)
    val jNull = jNullParser(P)
    val jBool = jBoolParser(P)

    val literal = jString | jNumber | jBool | jNull
    val value = literal // TODO: Add support for array and objects

    value
  }

  def jArrayParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    val open = char('[')
    val close = char(']')
    val comma = char(',')

    val jValue = jValueParser(P)

    val values = (jValue ** (comma ** jValue).manyL.map(_.map(_._2))).map {
      case (value, values) =>
        (value +: values).toIndexedSeq
    }

    val body = values.map(v => JArray(v))

    (open ** body ** close).map { case ((_, values), _) => values }
  }

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    // WIP, grammar at https://www.json.org/json-en.html

    val ws = whitespaceParser(P)
    val jString = jStringParser(P)
    val jValue = jValueParser(P)

    def jObject: Parser[JSON] = {
      val open = char('{')
      val close = char('}')
      val comma = char(',')
      val colon = char(':')

      val field = (ws ** jString ** ws ** colon ** jValue).map {
        case ((((_, name), _), _), value) =>
          name.get -> value
      }

      val fields = (field ** (comma ** field).manyL.map(_.map(_._2))).map {
        case (value, values) =>
          value +: values
      }

      val body = (ws.map(_ => List()) | (ws ** fields).map(_._2)).map { v =>
        JObject(v.toMap)
      }

      (open ** body ** close).map { case ((_, fields), _) => fields }
    }

    jObject
  }
}
