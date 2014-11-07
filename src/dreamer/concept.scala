package dreamer.concept
import scala.collection.GenTraversableOnce
import util.multimap._
import util.Util._


sealed abstract class Relation
object Relation {
  case object IsA extends Relation
  case object AtLocation extends Relation
  case object HasA extends Relation
  case class Verb(val verb: String) extends Relation
  case class NextTo(val direction: String) extends Relation
  case object HasState extends Relation
}

sealed abstract class QFragment[+T] {
  import Concept._
  def assign[R](f: T=>Option[Concept]): QFragment[T] = this match {
    case Variable(x) => f(x) match {
      case Some(value) => value
      case None => this
    }
    case _ => this
  }
}

sealed abstract class Concept extends QFragment[Nothing]
object Concept {
  case object Self extends Concept

  // TODO case class Assertion(val edge: Edge) extends Concept
  case class Abstract(val uri: String) extends Concept
  case class Realized(val id: Long) extends Concept
  val Thing = Abstract("/c/en/thing")
  val Place = Abstract("/c/en/place")
  val Nothingness = Abstract("/c/en/nothing")

  val DreamerGame = Abstract("/c/en/dreamer_game")

  // States
  val Sleeping = Abstract("/c/en/sleep")
  val Awake = Abstract("/c/en/awake")
  val Open = Abstract("/c/en/open")
  val Closed = Abstract("/c/en/close")

  // Variables are for querying and searching relations
  case class Variable[T](name: T) extends QFragment[T]
  val What = Variable(())
}

case class Edge(val start: Concept, val rel: Relation, val end: Concept)

case class Question[T](
    val start: QFragment[T],
    val rel: Relation,
    val end: QFragment[T]) {
  import Concept._

  def assign(f: T=>Option[Concept]): Question[T] =
    Question(start.assign(f), rel, end.assign(f))

  def toEdge(f: T=>Option[Concept]): Option[Edge] = assign(f) match {
    case Question(x: Concept, rel, y: Concept) => Some(Edge(x, rel, y))
    case _ => None
  }

  def map(f: QFragment[T]=>QFragment[T]): Question[T] =
    Question(f(start), rel, f(end))

  def flatMap(f: QFragment[T]=>GenTraversableOnce[QFragment[T]])
      : Set[Question[T]] =
    (for (s <- f(start).toArray; e <- f(end).toArray)
      yield Question(s, rel, e)).toSet

  def unify(e: Edge): Option[Map[T,Concept]] = {
    val Rel = e.rel
    val A = e.start
    val B = e.end
    this match {
      case Question(Variable(x), Rel, Variable(y)) =>
        Some(Map(x -> e.start, y -> e.end))
      case Question(A, Rel, Variable(y)) =>
        Some(Map(y -> e.end))
      case Question(Variable(x), Rel, B) =>
        Some(Map(x -> e.start))
      case Question(A, Rel, B) =>
        Some(Map[T,Concept]())
      case _ =>
        None
    }
  }
}


abstract class EdgeSource {
  type Self <: EdgeSource
  def ask[T](q: Question[T]): Set[Edge]
  def nameOf(c: Concept): Option[String]
  def named(name: String): Option[Concept]
  def name(c: Concept, name: String): Self

  // Search for a set of variable mappings that satisfy the given questions
  def search[T](qs: Question[T]*): Set[Map[T,Concept]] = {
    import Concept._


    def updateQuestions(mappings: Map[T,Concept], qs: List[Question[T]])
        : List[Question[T]] =
      qs match {
        case q::qs =>
          val mapped = q.assign(mappings.get)
          (mapped match {
            case Question(Variable(_), _, _) => mapped
            case Question(_, _, Variable(_)) => mapped
            // To make the best use of the memo/cache, keep one side as a var:
            case _ =>
              q match {
                case Question(Variable(_), _, _) =>
                  Question(q.start, q.rel, q.end.assign(mappings.get))
                case Question(_, _, Variable(_)) =>
                  Question(q.start.assign(mappings.get), q.rel, q.end)
                case _ => mapped // No choice but to query a full assertion
              }
          }) :: updateQuestions(mappings, qs)
        case Nil => Nil
      }

    def search1(mappings: Map[T,Concept], qs: List[Question[T]])
        : Set[Map[T,Concept]] = {
      qs match {
        case q::qs =>
          for (e <- ask(q);
               val unification = q.assign(mappings.get).unify(e);
               if !unification.isEmpty;
               val qs1 = updateQuestions(unification.get, qs);
               result <- search1(mappings ++ unification.get, qs1))
            yield result
        case _ =>
          Set(mappings)
      }
    }
    
    search1(Map(), List(qs:_*))
  }

  def searchWhat(q: Question[Unit]*): Set[Concept] = {
    search(q:_*).map(_.get(()).get)
  }
}

case class MentalMap(
    upstream: Option[EdgeSource] = None,
    byStart: MultiMap[(Relation,Concept), Edge] = MultiMap(),
    byEnd: MultiMap[(Relation,Concept), Edge] = MultiMap(),
    nameOfMap: Map[Concept,String] = Map(),
    namedMap: Map[String,Concept] = Map(),
    realizedCounter: Int=0)
    extends EdgeSource {
  import Concept._
  import Relation._
  
  type Self = MentalMap

  def nameOf(c: Concept): Option[String] =
    nameOfMap.get(c) orElse upstream.flatMap(_.nameOf(c))
  def named(name: String): Option[Concept] =
    namedMap.get(name) orElse upstream.flatMap(_.named(name))
  def name(c: Concept, name: String): MentalMap =
    this.copy(
      nameOfMap = nameOfMap + (c -> name),
      namedMap = namedMap + (name -> c))

  private def allocateRealized: (MentalMap,Realized) =
    (this.copy(realizedCounter = realizedCounter + 1),
        Realized(realizedCounter))

  def reify(concept: Concept): (MentalMap,Concept) = concept match {
    case archetype: Abstract =>
      val (this1, real) = allocateRealized
      (this1 + Edge(real, IsA, archetype), real)
    case _ => (this, concept)
  }

  def reify(concepts: GenTraversableOnce[Concept])
      : (MentalMap,Map[Concept,Concept]) =
    concepts.foldLeft((this,Map[Concept,Concept]()))((acc, concept) => {
      val (this1, cmap) = acc
      val (this2, realized) = this1.reify(concept)
      (this2, cmap + (concept -> realized))
    })

  def +(e: Edge) = this.copy(
    byStart = byStart + ((e.rel, e.start) -> e),
    byEnd = byEnd + ((e.rel, e.end) -> e))

  def -(e: Edge) = this.copy(
    byStart = byStart - ((e.rel, e.start) -> e),
    byEnd = byEnd - ((e.rel, e.end) -> e))

  private def badQuestion[T](q: Question[T]) =
    throw new IllegalArgumentException(q.toString)

  override def ask[T](q: Question[T]) = {
    import Concept._
    val local = q match {
      case Question(Variable(_), _, Variable(_)) => badQuestion(q)
      case Question(Variable(_), rel, end: Concept) =>
        byEnd.get((rel, end))
      case Question(start: Concept, rel, Variable(_)) =>
        byStart.get((rel, start))
      case Question(start: Concept, rel, end: Concept) =>
        byStart.get((rel, start)) & Set[Edge](Edge(start,rel,end))
      case _ =>
        assert(false, "QFragment that is neither a Var nor a Concept")
        badQuestion(q)
    }
    upstream match {
      case Some(up) => local | up.ask(q)
      case None => local
    }
  }

}

