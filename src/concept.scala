package dreamer.concept
import dreamer.adt._


sealed abstract class Relation
object Relation {
  case object IsA extends Relation
  case object AtLocation extends Relation
}

sealed abstract class Concept
object Concept {
  case object Self extends Concept
  case object Unknown extends Concept
  case object What extends Concept
  // TODO case class Assertion(val edge: Edge) extends Concept
  case class Abstract(val uri: String) extends Concept
  case class Realized(val id: Long) extends Concept
}

case class Edge(val start: Concept, val rel: Relation, val end: Concept)


trait EdgeSource {
  def ask(e: Edge): Set[Edge]
}

case class MentalMap(
    upstream: Option[EdgeSource] = None,
    byStart: MultiMap[(Relation,Concept), Edge] = MultiMap(),
    byEnd: MultiMap[(Relation,Concept), Edge] = MultiMap())
    extends EdgeSource {

  def +(e: Edge) = this.copy(
    byStart = byStart + ((e.rel, e.start) -> e),
    byEnd = byEnd + ((e.rel, e.end) -> e))

  def -(e: Edge) = this.copy(
    byStart = byStart - ((e.rel, e.start) -> e),
    byEnd = byEnd - ((e.rel, e.end) -> e))

  private def badQuestion(e: Edge) =
    throw new IllegalArgumentException(e.toString)

  override def ask(e: Edge) = {
    import Concept._
    val local = e match {
      case Edge(What, _, What) => badQuestion(e)
      case Edge(What, rel, end) => byEnd.get((rel, end))
      case Edge(start, rel, What) => byStart.get((rel, start))
      case Edge(start, rel, end) => byStart.get((rel, start)) & Set[Edge](e)
    }
    upstream match {
      case Some(up) => local | up.ask(e)
      case None => local
    }
  }
}

