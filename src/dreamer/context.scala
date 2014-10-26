package dreamer.context
import scala.util.Random
import scalaz._, Scalaz._
import util.forked._, ForkedState._
import dreamer.concept._
import util.Util._
import Concept._
import Relation._


case class Context(
    val mind: MentalMap,
    val r: Random=new Random,
    val refList: List[Context.Ref]=Nil,
    val it: Option[Context.Ref]=None) {

  def ref(ref: Context.Ref): Context = ref.real match {
    case Realized(_) =>
      this.copy(refList= ref :: refList)
    case _ => this
  }
}


object Context {
  case class Ref(val real: Concept, val arche: Concept)

  def ref(c: Concept): State[Context,Concept] = c match {
    case Realized(_) => for {
      ctx: Context <- init
      val ref = Ref(c, archetype(ctx,c))
      _ <- put(ctx.ref(ref))
    } yield c
    case _ => state(c)
  }

  def setIt(c: Concept): State[Context,Concept] = c match {
    case Realized(_) => for {
      ctx: Context <- init
      val ref = Ref(c, archetype(ctx,c))
      _ <- put(ctx.copy(it=Some(ref)))
    } yield c
    case _ => state(c)
  }

  def getIt: ForkedState[Context,Concept] =
    for (ctx <- fget; _ <- continueIf(!ctx.it.isEmpty))
      yield ctx.it.get.real

  def isIt(ctx: Context, c: Concept): Boolean = ctx.it match {
    case Some(ref) => ref.real == c
    case _ => false
  }

  def isReffed(c: Concept): State[Context,Boolean] = for {
    ctx <- init
  } yield isReffed(ctx, c)

  def isReffed(ctx: Context, c: Concept): Boolean = {
    (ctx.refList.map(_.real) contains c)
  }

  def archetype(ctx: Context, real: Concept): Concept = {
    val results = ctx.mind.ask(Question(real, IsA, What)).map(_.end)
    if (results.size == 0) Thing else ctx.r.shuffle(results.toList).head
  }
  
  def archetype[T](ctx: Context, qf: QFragment[T]): QFragment[T] =
    qf match {
      case x: Realized => archetype(ctx, x)
      case _ => qf
    }

  def archetype(real: Concept): State[Context,Concept] =
    for (ctx <- init) yield archetype(ctx, real)

  def archetype[T](qf: QFragment[T]): State[Context,QFragment[T]] =
    for (ctx <- init) yield archetype(ctx, qf)

  def ask[T](q: Question[T]): State[Context,List[Edge]] =
    State{ctx => (ctx, ctx.mind.ask(q).toList)}
  def search[T](qs: Question[T]*): State[Context,List[Map[T,Concept]]] =
    State{ctx => (ctx, ctx.mind.search(qs:_*).toList)}
  def searchWhat(qs: Question[Unit]*): State[Context,List[Concept]] =
    State{ctx => (ctx, ctx.mind.searchWhat(qs:_*).toList)}

  // Sometimes you can only reify one answer: e.g. if you ask where a house is
  // we want only one answer: street. Otherwise we could leave a house and
  // the dreamer would be in multiple locations
  private def reificationLimit[T](q: Question[T]) = q match {
    case Question(_, IsA, Variable(_)) => Some(1)
    case Question(_, AtLocation, Variable(_)) => Some(1)
    case _ => None
  }

  // Ask a question, but if there are no answers, then dream some up
  def reifyingAsk[T](q: Question[T]): State[Context,List[Edge]] =
    State(ctx => reifyingAsk(ctx, q))

  private def reifyingAsk[T](ctx: Context, q: Question[T]): (Context,List[Edge]) = {

    val current = ctx.mind.search(q)

    def defaultResult = 
      (ctx, for (mapping <- current.toList;
                 edge <- ctx.mind.ask(q.assign(mapping.get)))
              yield edge)
    
    def dreamUpResult: (Context,List[Edge]) = {
      debug("Question "+q.toString+" yielded no results")
      val absQ = q.map(archetype(ctx, _))
      debug("  Abstracted question: "+absQ.toString)
      val possibilities: List[Map[T,Concept]] = ctx.mind.search(absQ).toList
      debug("  Yielded "+possibilities.size+" results")

      // reify a random subset of them
      val count = reificationLimit(q) match {
        case Some(1) => 1
        case Some(x) => ctx.r.nextInt(x-1) + 1
        case None => ctx.r.nextInt(5) + 1
      }
      debug("  Reifying "+count)
      val archetypes: List[Map[T,Concept]] =
              ctx.r.shuffle(possibilities).take(count)
      debug("  They are: "+archetypes.toString)

      // reify and add them to the map
      archetypes.foldLeft((ctx,List[Edge]()))(
        (ctx_acc, mapping: Map[T,Concept]) => {
          val (ctx, acc) = ctx_acc
          val (mind, reifyMap) = ctx.mind.reify(mapping.values)
          val edge = q.toEdge(t =>
            for (abs <- mapping.get(t); real <- reifyMap.get(abs))
              yield real)
          (ctx.copy(mind=mind), edge match {
            case Some(x) => x::acc
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

  def reifyingSearch(q: Question[Unit]): State[Context,List[Concept]] = {
    require(q match {
      case Question(What,_,What) => false
      case Question(What,_,_) => true
      case Question(_,_,What) => true
      case Question(_,_,_) => false
    })
    for (edgeSet <- reifyingAsk(q))
      yield {
        for {
          edge <- edgeSet
          val unification = q.unify(edge)
          if !unification.isEmpty
        } yield unification.get.get(()).get
      }
  }

  def reifyingSearchAll(q: Question[Unit]): ForkedState[Context,List[Concept]] =
    fork(reifyingSearch(q))

  def reifyingSearch1(q: Question[Unit]): ForkedState[Context,Concept] =
    refork(reifyingSearch(q))

  def reify(c: Concept): State[Context,Concept] =
    State(ctx => reify(ctx, c))

  private def reify(ctx: Context, c: Concept): (Context, Concept) = {
    val (mind, real) = ctx.mind.reify(c)
    (ctx.copy(mind=mind), real)
  }

  def tell(e: Edge): State[Context,Edge] =
    State(ctx => tell(ctx, e))

  private def tell(ctx: Context, e: Edge): (Context, Edge) = {
    val mind = ctx.mind + e
    (ctx.copy(mind=mind), e)
  }

  def forget(e: Edge): State[Context,Unit] =
    State(ctx => (forget(ctx, e), ()))

  def forget(ctx: Context, e: Edge): Context = {
    val mind = ctx.mind - e
    ctx.copy(mind=mind)
  }

}
