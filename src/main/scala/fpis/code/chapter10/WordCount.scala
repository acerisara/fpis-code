package fpis.code.chapter10

import fpis.code.chapter10.Monoid.foldMapV

sealed trait WordCount
case class Stub(chars: String) extends WordCount
case class Part(lStub: String, words: Int, rStub: String) extends WordCount

object WordCount {

  val wordCountMonoid: Monoid[WordCount] = new Monoid[WordCount] {
    override def op(a1: WordCount, a2: WordCount): WordCount = (a1, a2) match {
      case (Part(lStub, words, rStub), Stub(chars)) =>
        Part(lStub, words, rStub + chars)
      case (Stub(chars), Part(lStub, words, rStub)) =>
        Part(chars + lStub, words, rStub)
      case (Stub(chars1), Stub(chars2)) =>
        Stub(chars1 + chars2)
      case (Part(lStub1, words1, rStub1), Part(lStub2, words2, rStub2)) =>
        Part(
          lStub1,
          words1 + words2 + (if ((rStub1 + lStub2).isEmpty) 0 else 1),
          rStub2
        )
    }

    override def zero: WordCount = Stub("")
  }

  def wordsCount(s: String): Int = {
    val m = wordCountMonoid

    def go(s: String): WordCount = {
      if (s.isEmpty) m.zero
      else if (s.length == 1) toWC(s.charAt(0))
      else {
        val (subL, subR) = s.splitAt(s.length / 2)
        m.op(go(subL), go(subR))
      }
    }

    go(s) match {
      case Stub(_)       => 0
      case Part(_, n, _) => n
    }
  }

  def wordsCountF(s: String): Int =
    foldMapV(s, wordCountMonoid)(toWC) match {
      case Stub(_)       => 0
      case Part(_, n, _) => n
    }

  def toWC(c: Char): WordCount =
    if (c == ' ') Part("", 0, "") else Stub(c.toString)

}
