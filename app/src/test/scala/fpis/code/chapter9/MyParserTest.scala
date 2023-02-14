package fpis.code.chapter9

import fpis.code.chapter9.JSON.JString
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

}
