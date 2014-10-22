package dreamer.conversation
import scalaz._, Scalaz._
import dreamer.concept._, Concept._, Relation._
import dreamer.context._, Context._


sealed abstract class Meaning
object Meaning {
  case class Ask(val question: Question[Unit]) extends Meaning
  case class Search(val questions: Question[Int]*) extends Meaning
  case class Tell(val edge: Edge) extends Meaning
  case object ParseFailure extends Meaning

  implicit def questionToAsk(q: Question[Unit]) = Ask(q)
  implicit def edgeToTell(e: Edge) = Tell(e)
}


object ParseUtil {
  def parser(implicit lang: Language): ContextM[Parser] =
    for (ctx <- get) yield lang.parser(ctx)

  def referent(text: String)(implicit lang: Language): ContextM[Set[Concept]] =
    for (p <- parser) yield p.referent(text)

  def abstractReferent(text: String)(implicit lang: Language): ContextM[Set[Concept]] =
    for (p <- parser) yield p.referent(text)

  def parse(text: String)(implicit lang: Language): ContextM[Set[Meaning]] =
    for (p <- parser) yield p.parse(text)

  def describe(concept: Concept)(implicit lang: Language): ContextM[String] =
    for (p <- parser) yield p.describe(concept)

  def describe(meaning: Meaning)(implicit lang: Language): ContextM[String] =
    for (p <- parser) yield p.describe(meaning)
}


trait Language {
  def parser(ctx: Context): Parser
}

trait Parser {
  import Meaning._

  def abstractReferent(text: String): Set[Concept]
  def referent(text: String): Set[Concept]

  protected def dictionary:
      Map[String,PartialFunction[List[String],Set[Meaning]]]

  protected val OK_CHARS = ' ' +: '\'' +:
      (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9'))

  protected def normalizeInput(text: String) =
    "\\s+".r.replaceAllIn(text.trim()," ").
        filter(OK_CHARS contains _).toLowerCase()

  // Returns all possible searches
  def parse(text: String): Set[Meaning] = {
    val norm = normalizeInput(text)
    for ((patt, translation) <- dictionary.toSet;
         val maybeMatches = patt.r.unapplySeq(norm);
         if !maybeMatches.isEmpty;
         val matches = maybeMatches.get;
         val _ = assert(translation isDefinedAt matches);
         meaning <- translation(matches))
      yield meaning
  }

  def describe(concept: Concept): String
  def describe(meaning: Meaning): String
}

case class Conversation(l: Language) {
  import ParseUtil._, Meaning._

  implicit val lang = l

  // FIXME right now, just echoes questions back
  def query(meaning: Meaning): ContextM[Meaning] =
    for (p <- parser)
      yield meaning

  protected def pure[A](ctx: Context, a: A): ContextM[A] =
    State(ctx => (ctx, a))

  def query(text: String): ContextM[String] =
    for (meanings <- parse(text);
         val ok = meanings.size == 1;
         ctx <- get;
         ans <- if (ok) query(meanings.head)
                else pure(ctx,ParseFailure);
         desc <- describe(ans))
      yield desc
}

