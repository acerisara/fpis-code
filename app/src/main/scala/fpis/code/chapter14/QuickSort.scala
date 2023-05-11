package fpis.code.chapter14

object QuickSort {

  def empty[S]: ST[S, Unit] = ST[S, Unit](())

  def partition[S](
      array: STArray[S, Int],
      n: Int,
      r: Int,
      pivot: Int
  ): ST[S, Int] = for {
    pivotVal <- array.read(pivot)
    _ <- array.swap(pivot, r)
    jRef <- STRef(n)
    _ <- (n until r).foldLeft(empty[S]) { (st, i) =>
      for {
        _ <- st
        elem <- array.read(i)
        _ <-
          if (elem < pivotVal) for {
            j <- jRef.read
            _ <- array.swap(i, j)
            _ <- jRef.write(j + 1)
          } yield ()
          else empty[S]
      } yield ()
    }
    j <- jRef.read
    _ <- array.swap(j, r)
  } yield j

  def qs[S](array: STArray[S, Int], n: Int, r: Int): ST[S, Unit] = if (n < r)
    for {
      p <- partition(array, n, r, n + (r - n) / 2)
      _ <- qs(array, n, p - 1)
      _ <- qs(array, p + 1, r)
    } yield ()
  else empty[S]

  def quicksort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) xs
    else
      ST.runST(new RunnableST[List[Int]] {
        def apply[S]: ST[S, List[Int]] = for {
          array <- STArray.fromList(xs)
          size <- array.size
          _ <- qs(array, 0, size - 1)
          sorted <- array.freeze
        } yield sorted
      })

}
