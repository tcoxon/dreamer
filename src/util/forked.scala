package util.forked
import scalaz._, Scalaz._


case class ForkedState[S,A](gen: S=>List[(S,A)]) {
  def apply(s: S): List[(S,A)] = gen(s)

  def flatMap[B](f: A=>ForkedState[S,B]): ForkedState[S,B] =
    ForkedState(s =>
      for {
        (s1,a) <- this(s)
        sb <- f(a)(s1)
      } yield sb
    )

  def map[B](f: A=>B): ForkedState[S,B] =
    ForkedState(s => this(s).map({case (s,a) => (s, f(a))}))

  def flatten[B](implicit asFSFS: ForkedState[S,A] <:< ForkedState[S,ForkedState[S,B]])
      : ForkedState[S,B] =
    asFSFS(this) flatMap identity

}

object ForkedState {
  // State-like functions:
  def state[S,A](a: A): ForkedState[S,A] = ForkedState(s => List((s,a)))
  def init[S]: ForkedState[S,S] = ForkedState(s => List((s,s)))
  def get[S]: ForkedState[S,S] = init
  def gets[S,T](f: S=>T): ForkedState[S,T] = ForkedState(s => List((s, f(s))))
  def put[S](s: S): ForkedState[S,Unit] = ForkedState(_ => List((s, ())))
  def modify[S](f: S=>S): ForkedState[S,Unit] =
    ForkedState {s => List((f(s),()))}

  // List-interop:
  def fork[S,A](la: List[A]): ForkedState[S,A] =
    ForkedState(s => la.map((s,_)))
  def fork[S,A](la: A*): ForkedState[S,A] = fork(la.toList)

  // State-interop:
  def fork[S,A](st: State[S,A]): ForkedState[S,A] =
    ForkedState(s => List(st(s)))
  def unfork[S,A](fs: ForkedState[S,A]): State[S,List[(S,A)]] =
    State{s => (s, fs(s))}
  def join[S,A](failure: List[(S,A)]=>State[S,A])(fs: ForkedState[S,A]): State[S,A] =
    State{s =>
      val r = fs(s)
      if (r.size == 1) r.head
      else failure(r)(s)
    }

  implicit def mpInstance[S] = new MonadPlus[({type L[A] = ForkedState[S,A]})#L] {
    def point[A](a: => A): ForkedState[S,A] = state(a)
    def bind[A, B](fa: ForkedState[S,A])(f: A => ForkedState[S,B])
        : ForkedState[S,B] =
      fa.flatMap(f)
    
    // Members declared in scalaz.Plus
    def plus[A](a: ForkedState[S,A], b: => ForkedState[S,A])
        : ForkedState[S,A] =
      ForkedState(s => a(s) ++ b(s))
    
    // Members declared in scalaz.PlusEmpty
    def empty[A]: ForkedState[S,A] = ForkedState(s => List())
  }
}
