package dreamer.lang
import scalaz._, Scalaz._
import util.forked._, ForkedState._
import dreamer.concept._, Concept._, Relation._
import dreamer.context._, Context._


object Language {
  type AmbiguousMeaning = ForkedState[Context,Response]
  type SpecificMeaning = State[Context,Response]
  type Translator = PartialFunction[List[String],AmbiguousMeaning]
  type Dictionary = Map[String,Translator]

  sealed abstract class NounPos
  case object SubjectPos extends NounPos
  case object ObjectPos extends NounPos

  sealed abstract class Response
  case class Tell(val edges: List[Edge]) extends Response
  case class Clarify(/*TODO possibility list*/) extends Response
  case class ParseFailure(/*TODO extra info*/) extends Response
  val Ack = Tell(Nil)
}

trait Language {
  import Language._

  // Language implementations must define these:
  def dictionary: State[Context,Dictionary]

  def describe(response: Response): State[Context,String]

  def describe(concept: Concept, pos: NounPos): State[Context,String]

  // Common to all language implementations:
  protected val OK_CHARS = ' ' +: '\'' +: '/' +:
      (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9'))

  protected def normalizeInput(text: String) =
    "\\s+".r.replaceAllIn(text.trim()," ").
        filter(OK_CHARS contains _).toLowerCase()

  // Returns all possible meanings
  def parse(text: String): AmbiguousMeaning = {
    val norm = normalizeInput(text)
    for {
      dict: Map[String,Translator] <- fork(dictionary)
      pair <- fork(dict.toList)
      val (patt, translation) = pair
      val maybeMatches: Option[List[String]] = patt.r.unapplySeq(norm)
      _ <- continueIf(!maybeMatches.isEmpty)
      val matches = maybeMatches.get
      _ <- continueIf(translation isDefinedAt matches)
      meaning:Response <- translation(matches)
    } yield meaning
  }

  private def clarify(ambiguity: List[(Context,Response)]): SpecificMeaning =
    state(if (ambiguity.size == 0) ParseFailure(/*TODO*/)
      else Clarify(/*TODO*/))

  def parseSpecific(text: String): SpecificMeaning =
    join(clarify)(parse(text))

}

