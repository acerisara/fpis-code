package fpis.code.chapter10

import fpis.code.chapter10.WordCount.{wordsCount, wordsCountF}
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class WordCountTest extends AnyFunSuite {

  test("Words count") {
    wordsCount("") should be(0)
    wordsCountF("") should be(0)

    wordsCount(" ") should be(0)
    wordsCountF(" ") should be(0)

    wordsCount("ab") should be(0)
    wordsCountF("ab") should be(0)

    wordsCount("a b") should be(0)
    wordsCountF("a b") should be(0)

    wordsCount("a word b") should be(1)
    wordsCountF("a word b") should be(1)

    wordsCount("word? word b") should be(1)
    wordsCountF("word? word b") should be(1)

    wordsCount("a word word?") should be(1)
    wordsCountF("a word word?") should be(1)

    wordsCount("a word word b") should be(2)
    wordsCountF("a word word b") should be(2)
  }

}
