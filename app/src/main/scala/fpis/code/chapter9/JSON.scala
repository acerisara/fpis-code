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
    (quote >> chars << quote).map(JString)
  }

  def jNumberParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    // Simplified version, doesn't support e
    val number = regex("(-?)(0|([1-9][0-9]*))(\\.[0-9]+)?".r)
    number.map(_.toDouble).map(JNumber)
  }

  def jNullParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    string("null").map(_ => JNull)
  }

  def jBoolParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    (string("true") | string("false")).map("true" == _).map(JBool)
  }

  def jValueParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    val ws = whitespaceParser(P)
    val jString = jStringParser(P)
    val jNumber = jNumberParser(P)
    val jNull = jNullParser(P)
    val jBool = jBoolParser(P)

    val literal = jString | jNumber | jBool | jNull
    // TODO: Add support for array and objects
    val value = literal

    ws >> value << ws
  }

  def jArrayParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    val open = char('[')
    val close = char(']')
    val comma = char(',')

    val ws = whitespaceParser(P)
    val jValue = jValueParser(P)

    val jValues = (ws >> comma >> jValue).manyL
    val values = (ws >> (jValue ++ jValues).map(_.toIndexedSeq).map(JArray).opt)
      .map(_.getOrElse(JArray(Vector.empty)))

    open >> values << close
  }

  def jFieldParser[Parser[+_]](P: Parsers[Parser]): Parser[(String, JSON)] = {
    import P._

    val colon = char(':')
    val comma = char(',')

    val ws = whitespaceParser(P)
    val jString = jStringParser(P)
    val jValue = jValueParser(P)

    (ws >> jString).map(
      _.get
    ) ** (ws >> colon >> jValue << comma.opt << ws)
  }

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    // WIP, grammar at https://www.json.org/json-en.html

    val open = char('{')
    val close = char('}')

    val ws = whitespaceParser(P)
    val jField = jFieldParser(P)

    val body = ws >> jField.manyL.opt.map(
      _.map(fields => JObject(fields.toMap)).getOrElse(JObject(Map.empty))
    )

    open >> body << close
  }
}
