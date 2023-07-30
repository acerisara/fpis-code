package fpis.code.chapter9

import fpis.code.chapter9.JSON._
import fpis.code.chapter9.MyParser.Parser
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MyParserTest extends AnyFunSuite {

  val myParser = new MyParser
  val jsonParser: Parser[JSON] = JSON.jsonParser(myParser)

  def parse(json: String): JSON =
    myParser.run(jsonParser)(json) match {
      case Left(e)  => throw new Exception(e.toString)
      case Right(v) => v
    }

  def obj(fields: Map[String, JSON]): JSON = JObject(fields)

  test("Empty object") {
    parse("{}") should be(emptyObject)
    parse("{  }") should be(emptyObject)
    parse(" {  } ") should be(emptyObject)

    parse("""
        |{
        |
        |
        |
        |}
        |""".stripMargin) should be(emptyObject)
  }

  test("Object with literals") {
    parse("""
        |{
        |  "a": 1.5123,
        |  "b": "abc",
        |  "c": true,
        |
        |  "d":    null,
        |  "e": false
        |
        |}""".stripMargin) should be(
      obj(
        Map(
          "a" -> JNumber(1.5123),
          "b" -> JString("abc"),
          "c" -> JBool(true),
          "d" -> JNull,
          "e" -> JBool(false)
        )
      )
    )
  }

  test("Object with array") {
    parse("""
        |{
        |  "a": []
        |}""".stripMargin) should be(
      obj(
        Map(
          "a" -> emptyArray
        )
      )
    )

    parse("""
        |{
        |  "a": [true, false, "abc", null, -152.234]
        |}""".stripMargin) should be(
      obj(
        Map(
          "a" -> JArray(
            Vector(
              JBool(true),
              JBool(false),
              JString("abc"),
              JNull,
              JNumber(-152.234)
            )
          )
        )
      )
    )

    parse("""
        |{
        |  "a": [true, [1, 2, 3]]
        |}""".stripMargin) should be(
      obj(
        Map(
          "a" -> JArray(
            Vector(
              JBool(true),
              JArray(
                Vector(
                  JNumber(1),
                  JNumber(2),
                  JNumber(3)
                )
              )
            )
          )
        )
      )
    )
  }

  test("Object with nested structure") {
    parse("""
        |{
        |  "a": {
        |    "b": 1,
        |    "c": true
        |  }
        |}""".stripMargin) should be(
      obj(
        Map(
          "a" -> obj(
            Map(
              "b" -> JNumber(1),
              "c" -> JBool(true)
            )
          )
        )
      )
    )
  }

}
