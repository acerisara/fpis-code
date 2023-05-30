package fpis.code.chapter15

import fpis.code.chapter15.process.Process._
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.junit.JUnitRunner

import java.io.{File, PrintWriter}
import java.util.concurrent.ExecutorService

@RunWith(classOf[JUnitRunner])
class ProcessTest extends AnyFunSuite {

  val es: ExecutorService = java.util.concurrent.Executors.newFixedThreadPool(4)

  test("Process.getLines") {
    def newFile(prefix: String): File = {
      val f = File.createTempFile(prefix, null)
      f.deleteOnExit()
      f
    }

    def write[A](content: List[A], f: File): Unit = {
      val writer = new PrintWriter(f)
      try content.foreach(writer.println(_))
      finally writer.close()
    }

    val input = newFile("lines")
    write(List("line1", "line2", "line3"), input)

    unsafePerformIO(runLog(getLines(input.getAbsolutePath)))(es) should be(
      Vector("line1", "line2", "line3")
    )
  }

}
