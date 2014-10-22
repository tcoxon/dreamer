package dreamer.context
import scala.util.Random
import scalaz._, Scalaz._
import dreamer.concept._
import dreamer.util.Util._
import Concept._
import Relation._


case class Context(
    val mind: MentalMap,
    val r: Random=new Random)


object Context {

  def archetype(ctx: Context, real: Concept): Concept = {
    val results = ctx.mind.ask(Question(real, IsA, What)).map(_.end)
    if (results.size == 0) Thing else ctx.r.shuffle(results.toList).head
  }
  
  def archetype[T](ctx: Context, qf: QFragment[T]): QFragment[T] =
    qf match {
      case x: Realized => archetype(ctx, x)
      case _ => qf
    }

  //def referent(ctx: Context)(label: String): Set[Concept]


  // Sometimes you can only reify one answer: e.g. if you ask where a house is
  // we want only one answer: street. Otherwise we could leave a house and
  // the dreamer would be in multiple locations
  def reificationLimit[T](q: Question[T]) = q match {
    case Question(_, IsA, Variable(_)) => Some(1)
    case Question(_, AtLocation, Variable(_)) => Some(1)
    case _ => None
  }

  // Ask a question, but if there are no answers, then dream some up
  def reifyingAsk[T](ctx: Context, q: Question[T]): (Context,Set[Edge]) = {

    val current = ctx.mind.search(q)

    def defaultResult = 
      (ctx, for (mapping <- current;
                 edge <- ctx.mind.ask(q.assign(mapping.get)))
              yield edge)
    
    def dreamUpResult: (Context,Set[Edge]) = {
      debug("Question "+q.toString+" yielded no results")
      val absQ = q.map(archetype(ctx, _))
      debug("  Abstracted question: "+absQ.toString)
      val possibilities: Set[Map[T,Concept]] = ctx.mind.search(absQ)
      debug("  Yielded "+possibilities.size+" results")

      // reify a random subset of them
      val count = reificationLimit(q) match {
        case Some(1) => 1
        case Some(x) => ctx.r.nextInt(x-1) + 1
        case None => ctx.r.nextInt(5) + 1
      }
      debug("  Reifying "+count)
      val archetypes: Set[Map[T,Concept]] =
              ctx.r.shuffle(possibilities).take(count)
      debug("  They are: "+archetypes.toString)

      // reify and add them to the map
      archetypes.foldLeft((ctx,Set[Edge]()))(
        (ctx_acc, mapping: Map[T,Concept]) => {
          val (ctx, acc) = ctx_acc
          val (mind, reifyMap) = ctx.mind.reify(mapping.values)
          val edge = q.toEdge(t =>
            for (abs <- mapping.get(t); real <- reifyMap.get(abs))
              yield real)
          (ctx.copy(mind=mind), edge match {
            case Some(x) => acc + x
            case None => acc
          })
        })
    }

    if (current.size > 0) {
      defaultResult
    } else q match {
      case Question(Realized(_), _, _) => dreamUpResult
      case Question(_, _, Realized(_)) => dreamUpResult
      case _ => defaultResult
    }
  }

  def reify(ctx: Context, c: Concept): (Context, Concept) = {
    val (mind, real) = ctx.mind.reify(c)
    (ctx.copy(mind=mind), real)
  }

  type ContextM[A] = State[Context,A]
  type ContextT[M[_],B] = StateT[M,Context,B]

  def toState[S,A](f: S=>(S,A)): State[S,A] =
    for (s: S <- get; val (s1, a) = f(s); _ <- put(s1)) yield a

  def reifyingAsk[T](q: Question[T]): ContextM[Set[Edge]] =
    toState(ctx => reifyingAsk(ctx, q))

  def reify(c: Concept): ContextM[Concept] =
    toState(ctx => reify(ctx, c))

  def pure[T](x: T): ContextM[T] = State(ctx => (ctx, x))
}
