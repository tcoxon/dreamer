package dreamer.conversation
import scalaz._, Scalaz._
import dreamer.concept._, Concept._, Relation._
import dreamer.context._, Context._
import dreamer.lang._


case class Conversation(l: Language) {
  import LangUtil._, Meaning._, Context._

  implicit val lang = l

  def queryMeaning(meaning: Meaning): ContextM[Meaning] = meaning match {

    case NoOp => pure(NoOp)
    case ParseFailure => pure(ParseFailure)
    case ClarifyWhich(_,_) => pure(meaning)
    case ClarifyWhat(_) => pure(meaning)

    case Ask(question, None) =>
      for (edgeSet <- reifyingAsk(question);
           val meanings = edgeSet.toList.map(Tell))
        yield Conjunction(meanings)

    case Ask(question, Some(followup)) =>
      for (edgeSet <- reifyingAsk(question);
           andThen <- edgeSet.toList match {
               case List() => pure(ClarifyWhat(question))
               case List(one) =>
                 val mapping: Option[Map[Unit,Concept]] = question.unify(one)
                 assert(!mapping.isEmpty && !mapping.get.get(()).isEmpty)
                 followup(mapping.get.get(()).get)
               case _ => pure(ClarifyWhich(question, edgeSet))
             };
           response <- queryMeaning(andThen))
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

