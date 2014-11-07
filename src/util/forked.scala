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
  def forked[S,A](a: A): ForkedState[S,A] = ForkedState(s => List((s,a)))
  def finit[S]: ForkedState[S,S] = ForkedState(s => List((s,s)))
  def fget[S]: ForkedState[S,S] = finit
  def fgets[S,T](f: S=>T): ForkedState[S,T] = ForkedState(s => List((s, f(s))))
  def fput[S](s: S): ForkedState[S,Unit] = ForkedState(_ => List((s, ())))
  def fmodify[S](f: S=>S): ForkedState[S,Unit] =
    ForkedState {s => List((f(s),()))}

  // List-interop:
  def fork[S,A](la: List[A]): ForkedState[S,A] =
    ForkedState(s => la.map((s,_)))

  // State-interop:
  def fork[S,A](st: State[S,A]): ForkedState[S,A] =
    ForkedState(s => List(st(s)))
  def unfork[S,A](fs: ForkedState[S,A]): State[S,List[(S,A)]] =
    State{s => (s, fs(s))}
  def refork[S,A](sl: State[S,List[A]]): ForkedState[S,A] =
    ForkedState{s =>
      val (s1,as) = sl(s)
      as.map((s1,_))
    }
  def join[S,A](failure: List[(S,A)]=>State[S,A])(fs: ForkedState[S,A]): State[S,A] =
    State{s =>
      val r = fs(s)
      if (r.size == 1) r.head
      else failure(r)(s)
    }

  def continueIf[S](cond: Boolean): ForkedState[S,Unit] =
    ForkedState(s => if (cond) List((s,())) else List())

  def cancelFork[S,A]: ForkedState[S,A] = ForkedState(s => Nil)

  implicit def mpInstance[S] = new MonadPlus[({type L[A] = ForkedState[S,A]})#L] {
    def point[A](a: => A): ForkedState[S,A] = forked(a)
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
  implicit def fInstance[S] = new Functor[({type L[A] = ForkedState[S,A]})#L] {
    def map[A,B](fa: ForkedState[S,A])(f: A=>B): ForkedState[S,B] = fa.map(f)
  }
}
