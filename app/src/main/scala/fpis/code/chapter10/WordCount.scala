package fpis.code.chapter10

import fpis.code.chapter10.Monoid.foldMapV

sealed trait WordCount
case class Stub(chars: String) extends WordCount
case class Part(lStub: String, words: Int, rStub: String) extends WordCount

object WordCount {

  val wordCountMonoid: Monoid[WordCount] = new Monoid[WordCount] {
    override def op(a1: WordCount, a2: WordCount): WordCount = (a1, a2) match {
      case (Part(l1, c1, r2), Stub(s)) => Part(l1, c1, r2 + s)
      case (Stub(s), Part(l2, c2, r2)) => Part(s + l2, c2, r2)
      case (Stub(s1), Stub(s2))        => Stub(s1 + s2)
      case (Part(l1, c1, r1), Part(l2, c2, r2)) =>
        Part(l1, c1 + c2 + (if ((r1 + l2).isEmpty) 0 else 1), r2)
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

  def wordsCountFM(s: String): Int =
    foldMapV(s, wordCountMonoid)(toWC) match {
      case Stub(_)       => 0
      case Part(_, n, _) => n
    }

  def toWC(c: Char): WordCount =
    if (c == ' ') Part("", 0, "") else Stub(c.toString)

}
