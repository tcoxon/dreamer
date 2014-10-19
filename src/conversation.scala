package dreamer.conversation
import scalaz._, Scalaz._
import dreamer.concept._, Concept._, Relation._
import dreamer.context._, Context._


trait Language {
  def abstractReferent(text: String): ContextM[Set[Concept]]
  def referent(text: String): ContextM[Set[Concept]]

  // Returns all possible searches
  def parse(text: String): ContextM[Set[List[Question[Unit]]]]

  def describe(q: Question[Unit]): ContextM[String]
  def describe(e: Edge): ContextM[String]
  def describe(qf: QFragment[Unit]): ContextM[String]
}

case class Conversation(
    val lang: Language) {

  // FIXME right now, just echoes questions back
  def query(text: String): ContextM[String] =
    for (queries <- lang.parse(text))
      yield queries.size match {
        case 0 => "What?"
        case 1 => queries.head.map(lang.describe).mkString(" && ")
        case _ => "Which?"
      }
}

