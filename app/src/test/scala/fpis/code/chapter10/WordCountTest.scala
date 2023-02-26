package fpis.code.chapter10

import fpis.code.chapter10.WordCount.{wordsCount, wordsCountFM}
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class WordCountTest extends AnyFunSuite {

  test("Words count") {
    wordsCount("") should be(0)
    wordsCountFM("") should be(0)

    wordsCount(" ") should be(0)
    wordsCountFM(" ") should be(0)

    wordsCount("ab") should be(0)
    wordsCountFM("ab") should be(0)

    wordsCount("a b") should be(0)
    wordsCountFM("a b") should be(0)

    wordsCount("a word b") should be(1)
    wordsCountFM("a word b") should be(1)

    wordsCount("word? word b") should be(1)
    wordsCountFM("word? word b") should be(1)

    wordsCount("a word word?") should be(1)
    wordsCountFM("a word word?") should be(1)

    wordsCount("a word word b") should be(2)
    wordsCountFM("a word word b") should be(2)
  }

}
