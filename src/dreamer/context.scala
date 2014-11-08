package dreamer.context
import scala.util.Random
import scala.collection.GenTraversableOnce
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
      this.copy(refList= (ref :: refList).distinct)
    case _ => this
  }
}


object Context {
  case class Ref(val real: Concept, val arche: Concept)

  var extraSpecification = false

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

  def clearIt: State[Context,Unit] = for {
    ctx:Context <- get
    val ctx1 = ctx.copy(it=None)
    _ <- put(ctx1)
  } yield ()

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
  def superkind[T](ctx: Context, qf: Concept): Concept =
    qf match {
      case x: Realized => {Thing}
      case x: Abstract =>
        val results = ctx.mind.searchWhat(Question(x, IsA, What))
        if (results.size == 0) qf else ctx.r.shuffle(results.toList).head
      case _ => qf
    }
  def superkind[T](ctx: Context, qf: QFragment[T]): QFragment[T] =
    qf match {
      case x: Concept => superkind(ctx, x)
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

  def isAwake(ctx: Context): Boolean =
    !ctx.mind.ask(Question(Self,HasState,Awake)).isEmpty

  // Sometimes you can only reify one answer: e.g. if you ask where a house is
  // we want only one answer: street. Otherwise we could leave a house and
  // the dreamer would be in multiple locations
  private def reificationLimit[T](q: Question[T]) = q match {
    case Question(_, IsA, Variable(_)) => Some(1)
    case Question(_, AtLocation, Variable(_)) => Some(1)
    case Question(_, NextTo(_), _) => Some(1)
    case _ => None
  }

  // Ask a question, but if there are no answers, then dream some up
  def reifyingAsk[T](q: Question[T]): State[Context,List[Edge]] =
    State(ctx => reifyingAsk(ctx, q))

  private def reifyingAsk[T](ctx: Context, q: Question[T]): (Context,List[Edge]) = {

    val current = ctx.mind.search(q)

    def defaultResult = 
      for {
        mapping <- current.toList
        edge <- ctx.mind.ask(q.assign(mapping.get))
      } yield edge
    
    // special reification control:
    def search(ctx: Context, q: Question[T]): Set[Map[T,Concept]] =
      q match {
        case Question(Variable(x), NextTo(_), Abstract(y)) =>
          Set(Map(x -> Abstract(y)))
        case _ => ctx.mind.search(q)
      }

    def dreamWeirdDefaults(ctx: Context, q: Question[T]): List[Map[T,Concept]] =
      // Called if there are no example AtLocations for somewhere.
      // We return random Things to fill the empty space.
      q match {
        case Question(Variable(x), AtLocation, _) =>
          ctx.mind.search(Question(Variable(x), IsA, Thing)).toList
        case _ => List()
      }

    def extendRealizations(e: Edge): List[Edge] =
      // Produce extra edges that must also exist for the new edge to be valid
      e match {
        case Edge(x,HasA,y) => Edge(y,AtLocation,x) :: e :: Nil
        case _ => e :: Nil
      }

    def dreamUpResult: (Context,List[Edge]) = {
      debug("Question "+q.toString+" yielded no results")
      val absQ = q.map(archetype(ctx, _))
      debug("  Abstracted question: "+absQ.toString)
      val possibilities0: List[Map[T,Concept]] = if (extraSpecification) {
        val absQ2 = q.map(qf => superkind(ctx, archetype(ctx, qf)))
        debug("  Even more abstract question: "+absQ2.toString)
        (search(ctx, absQ) | search(ctx, absQ2)).toList
      } else {
        search(ctx, absQ).toList
      }
      debug("  Yielded "+possibilities0.size+" results")

      val weird = possibilities0.size == 0 && ctx.r.nextBoolean
      val possibilities = if (weird) dreamWeirdDefaults(ctx, absQ)
          else possibilities0

      // reify a random subset of them
      val count = math.max((if (weird) 2 else reificationLimit(q) match {
        case Some(1) => 1
        case Some(x) => math.max(0, ctx.r.nextInt(x-2) + 2)
        case None => ctx.r.nextInt(4) + 2
      }) - current.size, 0)
      debug("  Reifying "+count)
      val archetypes: List[Map[T,Concept]] =
              ctx.r.shuffle(possibilities).take(count)
      debug("  They are: "+archetypes.toString)

      // reify and add them to the map
      archetypes.foldLeft((ctx,List[Edge]()))(
        (ctx_acc, mapping: Map[T,Concept]) => {
          val (ctx, acc) = ctx_acc
          val (ctx1, reifyMap) = reify(ctx, mapping.values)
          val mind = ctx1.mind
          val edge = q.toEdge(t =>
            for (abs <- mapping.get(t); real <- reifyMap.get(abs))
              yield real)
          val mind1 = edge match {
            case Some(e) => extendRealizations(e).foldLeft(mind)((a,b)=>a+b)
            case _ => mind
          }
          (ctx1.copy(mind=mind1), edge match {
            case Some(x) => x::acc
            case None => acc
          })
        })
    }

    val min = reificationLimit(q) match {
      case Some(x) => x-1
      case None => 2
    }
    if (current.size > min || isAwake(ctx)) {
      (ctx, defaultResult)
    } else q match {
      case Question(Realized(_), _, _) =>
        val (ctx1,r) = dreamUpResult
        (ctx1, defaultResult ++ r)
      case Question(_, _, Realized(_)) =>
        val (ctx1,r) = dreamUpResult
        (ctx1, defaultResult ++ r)
      case _ => (ctx, defaultResult)
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
    val arche = if (extraSpecification) {
      val specifics = ctx.mind.searchWhat(Question(What,IsA,c))
      if (extraSpecification && specifics.size > 4) {
        val specific = ctx.r.shuffle(specifics).head
        debug("Original arche was "+c.toString+"; making more specific "+
            specific.toString)
        specific
      } else c
    } else c

    val (mind, real) = ctx.mind.reify(arche)
    (ctx.copy(mind=mind), real)
  }

  private def reify(ctx: Context, concepts: GenTraversableOnce[Concept])
      : (Context, Map[Concept,Concept]) =
    concepts.foldLeft((ctx,Map[Concept,Concept]()))((acc, concept) => {
      val (ctx1, cmap) = acc
      val (ctx2, real) = reify(ctx1, concept)
      (ctx2, cmap + (concept -> real))
    })

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
