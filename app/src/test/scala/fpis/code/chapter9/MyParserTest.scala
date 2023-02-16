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

  test("Whitespace parser") {
    val ws = JSON.whitespaceParser(myParser)

    myParser.run(ws)("").isRight should be(true)
    myParser.run(ws)(" ").isRight should be(true)
    myParser.run(ws)("  ").isRight should be(true)

    myParser
      .run(ws)("""
        |
        |
        |""".stripMargin)
      .isRight should be(true)

    myParser
      .run(ws)("""
        |
        |
        |
        |
        |  """.stripMargin)
      .isRight should be(true)
  }

  test("jString parser") {
    val jString = JSON.jStringParser(myParser)

    myParser.run(jString)("\"\"") should be(Right(JString("")))
    myParser.run(jString)("\"a\"") should be(Right(JString("a")))
    myParser.run(jString)("\"abc\"") should be(Right(JString("abc")))
    myParser.run(jString)("\"abc1\"") should be(Right(JString("abc1")))
  }

  test("jNumber parser") {
    val jNumber = JSON.jNumberParser(myParser)

    myParser.run(jNumber)("0") should be(Right(JNumber(0)))
    myParser.run(jNumber)("0.5") should be(Right(JNumber(0.5)))
    myParser.run(jNumber)("-0.5") should be(Right(JNumber(-0.5)))
    myParser.run(jNumber)("1.52") should be(Right(JNumber(1.52)))
    myParser.run(jNumber)("-1.50") should be(Right(JNumber(-1.5)))
    myParser.run(jNumber)("10.501") should be(Right(JNumber(10.501)))
  }

  test("jNull parser") {
    val jNull = JSON.jNullParser(myParser)

    myParser.run(jNull)("null") should be(Right(JNull))
  }

  test("jBool parser") {
    val jBool = JSON.jBoolParser(myParser)

    myParser.run(jBool)("true") should be(Right(JBool(true)))
    myParser.run(jBool)("false") should be(Right(JBool(false)))
  }

  test("jArray parser") {
    val jArray = JSON.jArrayParser(myParser)

    myParser.run(jArray)("[1]") should be(
      Right(JArray(Vector(JNumber(1))))
    )

    myParser.run(jArray)("[1,2,3]") should be(
      Right(JArray(Vector(JNumber(1), JNumber(2), JNumber(3))))
    )

    myParser.run(jArray)("""["a","b"]""") should be(
      Right(JArray(Vector(JString("a"), JString("b"))))
    )

    myParser.run(jArray)("""[true,false]""") should be(
      Right(JArray(Vector(JBool(true), JBool(false))))
    )

    myParser.run(jArray)("""[null]""") should be(
      Right(JArray(Vector(JNull)))
    )

    myParser.run(jArray)("""[1,"a",true,null]""") should be(
      Right(JArray(Vector(JNumber(1), JString("a"), JBool(true), JNull)))
    )

    myParser.run(jArray)("[1, 2]") should be(
      Right(JArray(Vector(JNumber(1), JNumber(2))))
    )

    myParser.run(jArray)("[1 , 2]") should be(
      Right(JArray(Vector(JNumber(1), JNumber(2))))
    )

    myParser.run(jArray)("[1 , 2 ]") should be(
      Right(JArray(Vector(JNumber(1), JNumber(2))))
    )
  }

  test("Empty object") {
    val jObject = JSON.jsonParser(myParser)

    myParser.run(jObject)("{}") should be(Right(JObject(Map.empty)))
    myParser.run(jObject)("{  }") should be(Right(JObject(Map.empty)))

    myParser.run(jObject)("""{
        |
        |}""".stripMargin) should be(Right(JObject(Map.empty)))
  }

}