import scala.collection.immutable._


sealed abstract class Relation(val uri: String)
object Relation {
  case object IsA extends Relation("/r/IsA")
  case object AtLocation extends Relation("/r/AtLocation")
}

sealed abstract class Concept
object Concept {
  case object Self extends Concept
  case object Unknown extends Concept
  case object What extends Concept
  case class Generic(val uri: String) extends Concept
  case class RealObject(val id: Long) extends Concept
}

case class Edge(val start: Concept, val rel: Relation, val end: Concept)

case class MultiMap[K,V](map: Map[K,Set[V]] = Map[K,Set[V]]()) {
  def +(kv: (K,V)) = {
    val (k,v) = kv
    this.copy(
      map = map + (k -> (map.get(k) match {
        case Some(values) => values + v
        case None => Set(v)
      })))
  }

  def -(kv: (K,V)) = {
    val (k,v) = kv
    map.get(k) match {
      case Some(xs) => this.copy(map = map + (k -> (xs - v)))
      case None => this
    }
  }

  def get(k: K) = map.get(k) match {
    case Some(values) => values
    case None => Set()
  }
}

case class WorldGraph(
    byStart: MultiMap[(Relation,Concept), Edge] = MultiMap(),
    byEnd: MultiMap[(Relation,Concept), Edge] = MultiMap()) {

  def +(e: Edge) = this.copy(
    byStart = byStart + ((e.rel, e.start) -> e),
    byEnd = byEnd + ((e.rel, e.end) -> e))

  def -(e: Edge) = this.copy(
    byStart = byStart - ((e.rel, e.start) -> e),
    byEnd = byEnd - ((e.rel, e.end) -> e))

  private def badQuestion(e: Edge) =
    throw new IllegalArgumentException(e.toString)

  def ask(e: Edge) = {
    import Concept._
    e match {
      case Edge(What, _, What) => badQuestion(e)
      case Edge(What, rel, end) => byEnd.get((rel, end))
      case Edge(start, rel, What) => byStart.get((rel, start))
      case Edge(start, rel, end) => badQuestion(e)
    }
  }
  def askUpdate(e: Edge) = {
    val result = ask(e)
    if (result.isEmpty)
      (this, Set()) // TODO search conceptnet
    else
      (this, result)
  }
}


object Initialization {

  def initialWorld = {
    import Relation._
    import Concept._

    WorldGraph() + Edge(Self, IsA, Unknown)
  }

}

case class DreamerState(
  val world: WorldGraph,
  val hotEdges: Seq[Edge])

object Main {
  def main(args: Array[String]) = {
    import Concept._
    import Relation._
    val world = Initialization.initialWorld
    println(world.askUpdate(Edge(What,IsA,Unknown)))
    println(world.ask(Edge(Self,IsA,What)))
    println(world.ask(Edge(Self,AtLocation,What)))
    println(world.ask(Edge(What,AtLocation,What)))
  }
}
