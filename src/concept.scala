package dreamer.concept
import dreamer.adt._
import dreamer.util.Util._


sealed abstract class Relation
object Relation {
  case object IsA extends Relation
  case object AtLocation extends Relation
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
  // A sort of 'hard' unknown. Rather than fill in the relation randomly, the
  // system should report an outright 'I don't know'.
  case object Unknown extends Concept

  // TODO case class Assertion(val edge: Edge) extends Concept
  case class Abstract(val uri: String) extends Concept
  case class Realized(val id: Long) extends Concept

  // Variables are for querying and searching relations
  case class Variable[T](name: T) extends QFragment[T]
  val What = Variable(())
}

case class Edge(val start: Concept, val rel: Relation, val end: Concept)

case class Question[T](
    val start: QFragment[T],
    val rel: Relation,
    val end: QFragment[T]) {

  def assign(f: T=>Option[Concept]): Question[T] =
    Question(start.assign(f), rel, end.assign(f))

  def map(f: QFragment[T]=>QFragment[T]): Question[T] =
    Question(f(start), rel, f(end))
}


abstract class EdgeSource {
  def ask[T](q: Question[T]): Set[Edge]

  // Search for a set of variable mappings that satisfy the given questions
  def search[T](qs: Question[T]*): Set[Map[T,Concept]] = {
    import Concept._

    def unify(q: Question[T], e: Edge): Option[Map[T,Concept]] = {
      assert(q.rel == e.rel)
      val A = e.start
      val B = e.end
      q match {
        case Question(Variable(x), _, Variable(y)) =>
          Some(Map(x -> e.start, y -> e.end))
        case Question(A, _, Variable(y)) =>
          Some(Map(y -> e.end))
        case Question(Variable(x), _, B) =>
          Some(Map(x -> e.start))
        case Question(A, _, B) =>
          Some(Map[T,Concept]())
        case _ =>
          None
      }
    }

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
        : Set[Map[T,Concept]] =
      qs match {
        case q::qs =>
          for (e <- ask(q);
               val unification = unify(q.assign(mappings.get), e);
               if !unification.isEmpty;
               val qs1 = updateQuestions(unification.get, qs);
               result <- search1(mappings ++ unification.get, qs1))
            yield result
        case _ =>
          Set(mappings)
      }
    
    search1(Map(), List(qs:_*))
  }
}

case class MentalMap(
    upstream: Option[EdgeSource] = None,
    byStart: MultiMap[(Relation,Concept), Edge] = MultiMap(),
    byEnd: MultiMap[(Relation,Concept), Edge] = MultiMap(),
    realizedCounter: Int=0)
    extends EdgeSource {
  import Concept._

  def allocateRealized: (MentalMap,Realized) =
    (this.copy(realizedCounter = realizedCounter + 1),
        Realized(realizedCounter))

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

