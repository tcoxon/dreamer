package dreamer.lang.en
import scalaz._, Scalaz._
import util.forked._, ForkedState._
import dreamer.context._, Context._
import dreamer.concept._, Concept._, Relation._
import dreamer.lang._, Language._


class English extends Language {

  override def dictionary: State[Context,Dictionary] = state(Map(

    "look( around)?" -> {case _ =>
      for {
        here <- reifyingSearch1(Question(Self, AtLocation, What))
        things <- reifyingSearchAll(Question(What, AtLocation, here))
      } yield Tell(Edge(Self,AtLocation,here) ::
          things.filter(_ != Self).map(Edge(_,AtLocation,here)))
    }

  ))

  def describe(ctx: Context, concept: Concept, pos: NounPos): String =
    concept match {
      case Self => pos match {
        case SubjectPos => "I"
        case ObjectPos => "me"
      }
      case Abstract(uri) => uri // FIXME
      case r@Realized(_) =>
        "the "+describe(ctx, archetype(ctx, r), pos)
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

object EnglishTest {
  import dreamer.conversation._

  def main(args: Array[String]) = {
    def run: State[Context,String] =
      for (dog <- reify(Abstract("dog"));
           home <- reify(Abstract("home"));
           _ <- tell(Edge(Self,AtLocation,home));
           _ <- tell(Edge(dog,AtLocation,home));
           cat <- reify(Abstract("cat"));
           garden <- reify(Abstract("garden"));
           _ <- tell(Edge(cat,AtLocation,garden));
           resp <- Conversation(new English).query("look"))
        yield resp
    println(run(Context(MentalMap()))._2)
  }
}
