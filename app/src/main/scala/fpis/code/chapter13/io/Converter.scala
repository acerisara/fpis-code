package fpis.code.chapter13.io

import scala.io.StdIn.{readLine => readln}

object Converter {

  def readLine: IO[String] = IO { readln() }

  def printLine(msg: String): IO[Unit] = IO { println(msg) }

  def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0 / 9.0

  def converter: IO[Unit] = for {
    _ <- printLine("Enter a temperature in degrees Fahrenheit:")
    d <- readLine.map(_.toDouble)
    _ <- printLine(fahrenheitToCelsius(d).toString)
  } yield ()

  def main(args: Array[String]): Unit = converter.run

}
