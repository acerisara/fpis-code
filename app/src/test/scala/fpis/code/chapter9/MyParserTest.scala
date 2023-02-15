package fpis.code.chapter9

import fpis.code.chapter9.JSON.{JBool, JNull, JNumber, JString}
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

    myParser.run(ws)("") should be(Right(""))
    myParser.run(ws)(" ") should be(Right(""))
    myParser.run(ws)("  ") should be(Right(""))
    myParser.run(ws)("""
        |
        |
        |""".stripMargin) should be(Right(""))
    myParser.run(ws)("""
        |
        |
        |
        |
        |  """.stripMargin) should be(Right(""))
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

}
