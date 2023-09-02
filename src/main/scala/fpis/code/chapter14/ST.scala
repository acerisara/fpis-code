package fpis.code.chapter14

import scala.collection.mutable

sealed trait ST[S, A] { self =>

  protected def run(s: S): (A, S)

  def map[B](f: A => B): ST[S, B] = new ST[S, B] {
    def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }

  def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
    def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }
}

object ST {

  def apply[S, A](a: => A): ST[S, A] = {
    lazy val memo = a
    new ST[S, A] {
      def run(s: S): (A, S) = (memo, s)
    }
  }

  def runST[A](st: RunnableST[A]): A =
    st.apply[Unit].run(())._1

}

sealed trait STRef[S, A] {
  protected var cell: A
  def read: ST[S, A] = ST(cell)

  def write(a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S): (Unit, S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {
  def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(new STRef[S, A] {
    var cell: A = a
  })
}

trait RunnableST[A] {
  def apply[S]: ST[S, A]
}

sealed abstract class STArray[S, A: Manifest] {
  protected def array: Array[A]
  def size: ST[S, Int] = ST(array.length)

  def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S): (Unit, S) = {
      array(i) = a
      ((), s)
    }
  }

  def read(i: Int): ST[S, A] = ST(array(i))
  def freeze: ST[S, List[A]] = ST(array.toList)

  def fill(xs: Map[Int, A]): ST[S, Unit] = {
    xs.foldLeft(ST[S, Unit](())) { (st, v) =>
      for {
        _ <- st
        _ <- write(v._1, v._2)
      } yield ()
    }
  }

  def swap(i: Int, j: Int): ST[S, Unit] = for {
    x <- read(i)
    y <- read(j)
    _ <- write(i, y)
    _ <- write(j, x)
  } yield ()

}

object STArray {
  def apply[S, A: Manifest](sz: Int, a: A): ST[S, STArray[S, A]] = ST(
    new STArray[S, A] {
      lazy val array: Array[A] = Array.fill(sz)(a)
    }
  )

  def fromList[S, A: Manifest](as: List[A]): ST[S, STArray[S, A]] =
    ST(new STArray[S, A] {
      lazy val array: Array[A] = as.toArray
    })
}

sealed abstract class STMap[S, K, V] {
  protected def table: scala.collection.mutable.HashMap[K, V]
  def size: ST[S, Int] = ST(table.size)

  def get(key: K): ST[S, Option[V]] = ST(table.get(key))

  def put(key: K, value: V): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S): (Unit, S) = {
      table.put(key, value)
      ((), s)
    }
  }

}

object STMap {
  def apply[S, K, V](xs: (K, V)*): ST[S, STMap[S, K, V]] = ST(
    new STMap[S, K, V] {
      lazy val table: mutable.HashMap[K, V] = mutable.HashMap.from(xs)
    }
  )
}
