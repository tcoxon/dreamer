package dreamer.conversation
import scalaz._, Scalaz._
import dreamer.concept._, Concept._, Relation._
import dreamer.context._, Context._

object Language {
  type Phrase = List[Question[Unit]]
  object Phrase {
    def apply(qs: Question[Unit]*) = List(qs:_*)
  }

  def parser(implicit lang: Language): ContextM[Parser] =
    for (ctx <- get) yield lang.parser(ctx)
}

trait Language {
  def parser(ctx: Context): Parser
}

trait Parser {
  import Language._

  def abstractReferent(text: String): Set[Concept]
  def referent(text: String): Set[Concept]

  protected def dictionary:
      Map[String,PartialFunction[List[String],Set[Phrase]]]

  protected val OK_CHARS = ' ' +: '\'' +:
      (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9'))

  protected def normalizeInput(text: String) =
    "\\s+".r.replaceAllIn(text.trim()," ").
        filter(OK_CHARS contains _).toLowerCase()

  // Returns all possible searches
  def parse(text: String): Set[Phrase] = {
    val norm = normalizeInput(text)
    for ((patt, meaning) <- dictionary.toSet;
         val maybeMatches = patt.r.unapplySeq(text);
         if !maybeMatches.isEmpty;
         val matches = maybeMatches.get;
         if meaning isDefinedAt matches;
         phrase <- meaning(matches))
      yield {
        phrase
      }
  }

  def describe(q: Question[Unit]): String
  def describe(e: Edge): String
  def describe(qf: QFragment[Unit]): String
}

case class Conversation(
    val lang: Language) {
  import Language._

  protected def p(ctx: Context) = lang.parser(ctx)

  // FIXME right now, just echoes questions back
  def query(phrase: Phrase): ContextM[String] =
    for (ctx <- init)
      yield phrase.map(p(ctx).describe).mkString(" && ")

  def query(text: String): ContextM[String] = State{ ctx: Context =>
    p(ctx).parse(text).toList match {
      case List(phrase) => query(phrase)(ctx)
      case _ => (ctx, "I don't understand.")
    }
  }
}

