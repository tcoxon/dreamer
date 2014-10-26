package dreamer.lang.en
import scalaz._, Scalaz._
import util.forked._, ForkedState._
import dreamer.context._, Context._
import dreamer.concept._, Concept._, Relation._
import dreamer.lang._, Language._
import dreamer.game._, Game._


class English extends Language {

  override def dictionary: State[Context,Dictionary] = state(Map(

    "(take a )?look( around.*)?" -> {case _ => fork(lookAround)},

    "(take a )?look( at)? (.+)" -> {case List(_, _, x) =>
      for {
        xref <- referent(x)
        _ <- fork(setIt(xref))
        r <- fork(lookAt(xref))
      } yield r
    },
    
    "leave( here| this place)?" -> {case _ => fork(leave)},

    "leave (.+)" -> {case List(x) =>
      for {
        xref <- referent(x)
        _ <- fork(setIt(xref))
        r <- fork(leave(xref))
      } yield r
    }

  ))


  def referent(text: String): ForkedState[Context,Concept] = text match {
    case "you" => forked(Self)
    case "yourself" => forked(Self)
    case "it" => for (x <- getIt) yield x
    case _ =>
      // TODO look up names of Abstracts rather than using uris
      val Uri = "(a|an|the) ".r.replaceAllIn(text, "")
      for {
        ctx:Context <- fget
        val ref = ctx.refList.find(r => r.arche match {
          case Abstract(Uri) => true
          case _ => false
        })
        _ <- continueIf(!ref.isEmpty)
      } yield ref.get.real
  }

  def describeUnqual(concept: Concept, pos: NounPos): State[Context,String] =
    concept match {
      case Self => state(pos match {
        case SubjectPos => "I"
        case ObjectPos => "me"
      })
      case Abstract(uri) => state(uri) // FIXME TODO use name rather than uri
      case r@Realized(_) =>
        for {
          arche <- archetype(r)
          desc <- describeUnqual(arche, pos)
        } yield desc
    }

  def singular(noun: String): String =
    if (noun.length > 0 && "aeiou".contains(noun.charAt(0))) "an "+noun
    else "a "+noun

  def describe(concept: Concept, pos: NounPos): State[Context,String] =
    concept match {
      case Self => describeUnqual(concept, pos)
      case Abstract(_) => describeUnqual(concept,pos).map(singular)
      case Realized(_) =>
        for {
          desc <- describeUnqual(concept, pos)
          ctx <- get
        } yield
          if (isIt(ctx, concept)) "it" else
            (if (isReffed(ctx, concept)) "the "+desc else singular(desc))
    }

  private def describeV(subj: Concept, rel: Relation): String =
    " " + (rel match {
      case IsA => subj match {
        case Self => "am"
        case _ => "is"
      }
      case AtLocation => subj match {
        case Self => "am in"
        case _ => "is in"
      }
    }) + " "

  private def describeSVO(edge: Edge): State[Context,String] =
    for {
      sdesc <- describe(edge.start, SubjectPos)
      odesc <- describe(edge.end, ObjectPos)
      _ <- ref(edge.start)
      _ <- ref(edge.end)
      _ <- setIt(edge.start)
    } yield sdesc + describeV(edge.start, edge.rel) + odesc

  def describe(edge: Edge): State[Context,String] = edge match {
    case Edge(_, IsA, _) => describeSVO(edge)
    case Edge(x, AtLocation, y) => describeSVO(edge)
  }

  private def sentence(text: String): String =
    text.capitalize + "."

  def describe(edges: List[Edge]): State[Context,List[String]] = edges match {
    case Nil => state(Nil)
    case e::edges => 
      for (edesc <- describe(e); esdesc <- describe(edges))
        yield sentence(edesc) :: esdesc
  }

  def describe(response: Response): State[Context,String] = response match {
    case Ack => state("OK.")
    case Tell(es) => for (descs <- describe(es)) yield descs.mkString(" ")
    case Clarify() => state("Can you clarify that?")
    case ParseFailure() => state("I don't understand.")
  }

}

