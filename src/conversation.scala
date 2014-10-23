package dreamer.conversation
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
      val followup: Option[Edge=>ContextM[Meaning]]=None) extends Meaning
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

  def abstractReferent(text: String): Set[Concept]
  def referent(text: String): Set[Concept]

  def dictionary: Map[String,PartialFunction[List[String],Set[Meaning]]]

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

case class Conversation(l: Language) {
  import LangUtil._, Meaning._, Context._

  implicit val lang = l

  def queryMeaning(meaning: Meaning): ContextM[Meaning] = meaning match {

    case NoOp => pure(NoOp)
    case ParseFailure => pure(ParseFailure)
    case ClarifyWhich(_,_) => pure(ParseFailure)
    case ClarifyWhat(_) => pure(ParseFailure)

    case Ask(question, None) =>
      for (edgeSet <- reifyingAsk(question);
           val meanings = edgeSet.toList.map(Tell))
        yield Conjunction(meanings)

    case Ask(question, Some(followup)) =>
      for (edgeSet <- reifyingAsk(question);
           response <- edgeSet.toList match {
               case List() => pure(ClarifyWhat(question))
               case List(one) => followup(one)
               case _ => pure(ClarifyWhich(question, edgeSet))
             })
        yield response

    //case Search(question, None) =>
    //case Search(question, Some(followup)) =>

    case Reify(archetype, None) =>
      for (real <- reify(archetype))
        yield Tell(Edge(real,IsA,archetype))

    case Reify(archetype, Some(followup)) =>
      for (real <- reify(archetype);
           response <- followup(real))
        yield response

    case Tell(edge) =>
      for (ctx <- get;
           _ <- put(ctx.copy(mind = ctx.mind + edge)))
        yield edge

    case Conjunction(Nil) => pure(NoOp)
    case Conjunction(head::tail) =>
      for (headResponse <- queryMeaning(head);
           tailResponse <- queryMeaning(Conjunction(tail));
           val tailR = tailResponse match {
             case Conjunction(ls) => ls
             case NoOp => List()
             case x@_ => List(x)
           })
        yield Conjunction(headResponse::tailR)
  }

  def queryMeaning(text: String): ContextM[Meaning] =
    for (meaning <- parseOne(text);
         response <- queryMeaning(meaning))
      yield response

  def query(text: String): ContextM[String] =
    for (response <- queryMeaning(text);
         desc <- describe(response))
      yield desc
}

