import org.scalatest._
import scalaz._, Scalaz._
import dreamer.concept._, Concept._, Relation._
import dreamer.context._, Context._
import dreamer.conversation._, Language._

class TestLanguage extends Language {

  def parser(ctx: Context) = new Parser {
    def abstractReferent(text: String) =
      referent(text).map(archetype(ctx,_))
    
    def referent(text: String) =
      for (mapping <- ctx.mind.search(Question(What,IsA,Abstract(text)));
           if mapping.size > 0;
           if (mapping.values.head match {
                case Realized(_) => true
                case _ => false});
           val ref = mapping.get(());
           if !ref.isEmpty)
        yield {
          assert(mapping.size == 1)
          ref.get
        }

    override def dictionary = Map(
        "what is (.*)" -> {case List(x) =>
          for (xref <- referent(x))
            yield Phrase(Question(xref,IsA,What))
        },
        "where is (.*)" -> {case List(x) =>
          for (xref <- referent(x))
            yield Phrase(Question(xref,AtLocation,What))
        }
      )

    def describe(q: Question[Unit]) = q.toString
    def describe(e: Edge) = e.toString
    def describe(qf: QFragment[Unit]) = qf.toString
  }
}


class ConversationSuite extends FunSuite {
  
  val ctx = Context(MentalMap())
  implicit val lang = new TestLanguage()

  test("Referring to dog should give the realized dog") {
    def run =
      for (dog <- reify(Abstract("dog"));
           p <- parser) yield {
        assert(p.referent("dog") == Set(dog))
      }
    run(ctx)
  }
  test("Asking for the abstractReferent of dog should give the abstract dog") {
    def run =
      for (dog <- reify(Abstract("dog"));
           p <- parser) yield {
        assert(p.abstractReferent("dog") == Set(Abstract("dog")))
      }
    run(ctx)
  }
  test("Parsing 'what is dog' should give the right question") {
    val conv = Conversation(lang)
    def run =
      for (dog_ <- reify(Abstract("dog"));
           val dog = dog_.asInstanceOf[Realized];
           p <- parser) yield {
        val result = p.parse("What is dog?")
        assert(result == Set(List(Question(dog,IsA,What))))
      }
    run(ctx)
  }

}
