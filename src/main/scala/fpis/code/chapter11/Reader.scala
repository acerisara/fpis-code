package fpis.code.chapter11

case class Reader[R, A](run: R => A)

object Reader {

  def readerMonad[R]: Monad[
    ({
      type f[x] = Reader[R, x]
    })#f
  ] = new Monad[({ type f[x] = Reader[R, x] })#f] {
    override def unit[A](a: => A): Reader[R, A] = Reader(_ => a)

    override def flatMap[A, B](
        st: Reader[R, A]
    )(f: A => Reader[R, B]): Reader[R, B] =
      Reader { r =>
        f(st.run(r)).run(r)
      }
  }
}
