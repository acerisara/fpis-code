package fpis.code.chapter6

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = State(s => {
    val (a, s2) = run(s)
    (f(a), s2)
  })

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s2) = run(s)
    f(a).run(s2)
  })

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap { a =>
      sb.map { b =>
        f(a, b)
      }
    }

}

object State {

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit(List.empty[A]): State[S, List[A]])((sa, sas) =>
      sa.map2(sas)(_ :: _)
    )

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

}
