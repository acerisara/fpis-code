package fpis.code.chapter9

trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  val emptyArray: JArray = JArray(IndexedSeq.empty)
  val emptyObject: JObject = JObject(Map.empty)

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    // WIP, grammar at https://www.json.org/json-en.html

    val curlyBracketOpen = char('{')
    val curlyBracketClosed = char('}')
    val space = char(' ')
    val linefeed = char('\n')
    val carriageReturn = char('\r')
    val horizontalTab = char('\t')
    val quote = char('"')
    val chars = regex("\\w*".r)
    val colon = char(':')
    val comma = char(',')
    val squareBracketOpen = char('[')
    val squareBracketClosed = char(']')

    val ws = (space | linefeed | carriageReturn | horizontalTab).many
      .map(_ => ())

    val jString = (quote >> chars << quote).map(JString)

    val jNumber =
      regex("(-?)(0|([1-9][0-9]*))(\\.[0-9]+)?".r).map(_.toDouble).map(JNumber)

    val jNull = string("null").map(_ => JNull)

    val jBoolean =
      (string("true") | string("false")).map("true" == _).map(JBool)

    val jLiteral: Parser[JSON] = jString | jNumber | jNull | jBoolean

    def jValue: Parser[JSON] = jLiteral | jArray | jObject

    def jArray: Parser[JArray] = {
      val jValues = (comma >> ws >> jValue).many

      val values =
        (jValue ++ jValues)
          .map(_.toVector)
          .map(JArray)
          .opt
          .map(_.getOrElse(emptyArray))

      squareBracketOpen >> values << squareBracketClosed
    }

    def jField: Parser[(String, JSON)] =
      jString.map(_.get) ** (colon >> ws >> jValue << comma.opt << ws)

    def jObject: Parser[JObject] = {
      val body = jField.many.opt.map(
        _.map(fields => JObject(fields.toMap)).getOrElse(emptyObject)
      )

      ws >> curlyBracketOpen >> ws >> body << curlyBracketClosed << ws
    }

    jObject
  }
}
