import scala.util.Random
import dreamer.concept._
import dreamer.conceptnet._
import dreamer.util.Util._
import Concept._
import Relation._


object Main {
  val rootConcept = "/c/en/thing"

  case class State(val mind: MentalMap, val r: Random)

  def realToAbstract[T](s: State)(c: QFragment[T]) = c match {
    case x@Realized(_) =>
      val abs = s.mind.ask(Question(x,IsA,What)).map(_.end)
      if (abs.size == 0) Abstract(rootConcept)
      else {
        val arr = abs.toArray
        arr(s.r.nextInt(abs.toArray.size))
      }
    case x@_ => x
  }

  def fillBlanks(q: Question[Unit], c: Concept): Edge = q match {
    case Question(What, rel, What) => Edge(c, rel, c)
    case Question(What, rel, end: Concept) => Edge(c, rel, end)
    case Question(start: Concept, rel, What) => Edge(start, rel, c)
    case Question(start: Concept, rel, end: Concept) => Edge(start, rel, end)
    case Question(_,_,_) =>
      assert(false)
      null
  }

  def takeBlanks(es: List[Edge], q: Question[Unit]): List[Concept] = es match {
    case e::es =>
      q match {
        case Question(What, _, _) => e.start :: takeBlanks(es, q)
        case Question(_, _, What) => e.end :: takeBlanks(es, q)
        case _ => takeBlanks(es, q)
      }
    case _ => Nil
  }

  def reifyingAsk[T](s: State, q: Question[Unit]): (State,Set[Edge]) = {

    val res = s.mind.ask(q)
    if (res.size > 0) {
      (s, res)
    } else if (q.start == Self) {
      // TODO specials, e.g. ask What,IsA,/c/en/place
      (s, res)
    } else if (q.end == Self) {
      (s, res)
    } else {
      debug("Question "+q.toString+" yielded no results")
      val absQ = q.map(realToAbstract(s))
      debug("  Abstracted question: "+absQ.toString)
      val possibilities = s.mind.ask(absQ).toList
      debug("  Yielded "+possibilities.size+" results")
      // reify a random subset of them
      val count = s.r.nextInt(5) + 1
      debug("  Reifying "+count)
      val absObjs = takeBlanks(s.r.shuffle(possibilities).take(count), q)
      debug("  They are: "+absObjs.toString)
      val (s1, realObjs) = absObjs.foldLeft((s,List[Realized]()))(
          (s_acc, abs) => {
            val (s,acc) = s_acc
            val (mind1, real) = s.mind.allocateRealized
            val s1 = s.copy(mind = mind1)
            val s2 = s1.copy(mind = s1.mind + Edge(real,IsA,abs))
            (s1, acc :+ real)
          })
      debug("  IDs: "+realObjs.toString)
      realObjs.foldLeft((s1,Set[Edge]()))((s_acc, real) => {
          val (s,acc) = s_acc
          val result = fillBlanks(q, real)
          (s.copy(mind = s.mind + result), acc + result)
      })
    }
  }

  def main(args: Array[String]) {
    val mind0 = MentalMap(Some(new ConceptNet())) + Edge(Self,IsA,Unknown)
    val (mind1, house) = mind0.allocateRealized
    val (mind2, mountain) = mind1.allocateRealized
    val mind3 = mind2 + Edge(house,IsA,Abstract("/c/en/house"))
    val mind4 = mind3 + Edge(mountain,IsA,Abstract("/c/en/mountain"))
    val state = State(mind4, new Random())
    reifyingAsk(state, Question(What,AtLocation,house))
    reifyingAsk(state, Question(mountain,AtLocation,What))
    reifyingAsk(state, Question(What,AtLocation,mountain))
    
    println("Hi")
  }
}
