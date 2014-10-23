package dreamer.lang.en
import scalaz._, Scalaz._
import dreamer.context._, Context._
import dreamer.concept._, Concept._, Relation._
import dreamer.lang._, Meaning._


class English extends Language {

  def parser(ctx: Context) = new Parser {
    
    val dictionary: Dictionary =
      Map(
        "look( around)?" -> {case _ =>
          Set(Ask(Question(Self,AtLocation,What),
            Some{here: Concept =>
              pure(Ask(Question(What,AtLocation,here)))
            }))
        }
      )

    def abstractReferent(text: String): Set[Concept] = Set()
    def referent(text: String): Set[Concept] = Set()
  }

  def describe(concept: Concept) = pure(concept.toString)
    //concept match {
    //}
  def describe(meaning: Meaning) = pure(meaning.toString)
    //meaning match {
    //}

}

object EnglishTest {
  import dreamer.conversation._

  def main(args: Array[String]) = {
    def run: ContextM[String] =
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
