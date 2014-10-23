package dreamer.lang
import scalaz._, Scalaz._
import dreamer.concept._, Concept._, Relation._
import dreamer.context._, Context._


sealed abstract class Meaning
object Meaning {
  case object NoOp extends Meaning
  case object ParseFailure extends Meaning
  case class ClarifyWhich(val question: Question[Unit],
      val edges: Set[Edge]) extends Meaning
  case class ClarifyWhat(val question: Question[Unit]) extends Meaning

  case class Ask(val question: Question[Unit],
      val followup: Option[Concept=>ContextM[Meaning]]=None) extends Meaning {

    assert(question match {
      case Question(What,_,What) => false
      case Question(What,_,_) => true
      case Question(_,_,What) => true
      case Question(_,_,_) => false
    })

  }
  //case class Search(val questions: Seq[Question[Int]],
  //    val followup: Option[Map[Int,Concept]=>ContextM[Meaning]]=None) extends Meaning
  case class Reify(val archetype: Concept,
      val followup: Option[Concept=>ContextM[Meaning]]=None) extends Meaning
  case class Tell(val edge: Edge) extends Meaning
  case class Conjunction(val sub: List[Meaning]) extends Meaning

  implicit def questionToAsk(q: Question[Unit]) = Ask(q)
  implicit def edgeToTell(e: Edge) = Tell(e)

  def flattenMeaning(meaning: Meaning): List[Meaning] =
    // Remove all conjunctions
    meaning match {
      case Conjunction(ls) =>
        for (m <- ls; m0 <- flattenMeaning(m)) yield m0
      case _ => List(meaning)
    }
}


object LangUtil {
  def parser(implicit lang: Language): ContextM[Parser] =
    for (ctx <- get) yield lang.parser(ctx)

  def referent(text: String)(implicit lang: Language): ContextM[Set[Concept]] =
    for (p <- parser) yield p.referent(text)

  def abstractReferent(text: String)(implicit lang: Language): ContextM[Set[Concept]] =
    for (p <- parser) yield p.referent(text)

  def parse(text: String)(implicit lang: Language): ContextM[Set[Meaning]] =
    for (p <- parser) yield p.parse(text)

  def parseOne(text: String)(implicit lang: Language): ContextM[Meaning] =
    for (p <- parser) yield p.parseOne(text)

  def describe(concept: Concept)(implicit lang: Language): ContextM[String] =
    for (desc <- lang.describe(concept)) yield desc

  def describe(meaning: Meaning)(implicit lang: Language): ContextM[String] =
    for (desc <- lang.describe(meaning)) yield desc
}


trait Language {
  def parser(ctx: Context): Parser

  def describe(concept: Concept): ContextM[String]
  def describe(meaning: Meaning): ContextM[String]
}

trait Parser {
  import Meaning._
  type Dictionary = Map[String,PartialFunction[List[String],Set[Meaning]]]

  def abstractReferent(text: String): Set[Concept]
  def referent(text: String): Set[Concept]

  def dictionary: Dictionary

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
  def parseOne(text: String): Meaning = parse(text).toList match {
    case List(meaning) => meaning
    case _ => ParseFailure
  }

}
