import scala.util.Random
import dreamer.concept._
import dreamer.conceptnet._
import dreamer.util.Util._
import Concept._
import Relation._


object Main {
  val Thing = Abstract("/c/en/thing")

  case class Context(
      val mind: MentalMap,
      val r: Random)

  def archetype(ctx: Context, real: Realized): Concept = {
    val results = ctx.mind.ask(Question(real, IsA, What)).map(_.end)
    if (results.size == 0) Thing else ctx.r.shuffle(results.toList).head
  }
  
  def archetype[T](ctx: Context, qf: QFragment[T]): QFragment[T] =
    qf match {
      case x: Realized => archetype(ctx, x)
      case _ => qf
    }

  //def referent(ctx: Context)(label: String): Set[Concept]


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
      val count = ctx.r.nextInt(5) + 1
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


  def main(args: Array[String]) {
    val mind0 = MentalMap(Some(new ConceptNet())) + Edge(Self,IsA,Unknown)
    val (mind1, house) = mind0.reify(Abstract("/c/en/house"))
    val (mind2, mountain) = mind1.reify(Abstract("/c/en/mountain"))
    val state = Context(mind2, new Random())
    reifyingAsk(state, Question(What,AtLocation,house))
    reifyingAsk(state, Question(mountain,AtLocation,What))
    reifyingAsk(state, Question(What,AtLocation,mountain))
    
    println("Hi")
  }
}
