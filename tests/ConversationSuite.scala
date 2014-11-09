import org.scalatest._
import scalaz._, Scalaz._
import util.forked._, ForkedState._
import dreamer.concept._, Concept._, Relation._
import dreamer.context._, Context._
import dreamer.conversation._
import dreamer.lang._, Language._

class TestLanguage extends Language {

  def dictionary: State[Context,Dictionary] =
    state(Map(
      "what is (.*)" -> {case List(x) =>
        for {
          xref <- referent(x)
          xArches <- reifyingSearchAll(Question(xref,IsA,What))
        } yield Tell(xArches.map(Edge(xref, IsA, _)))
      },
      "where is (.*)" -> {case List(x) =>
        for {
          xref <- referent(x)
          loc <- reifyingSearch1(Question(xref,AtLocation,What))
        } yield Tell(Edge(xref, AtLocation, loc) :: Nil)
      },
      "there is (.*)" -> {case List(x) =>
        for {
          xref <- abstractReferent(x)
          _ <- fork(reify(xref))
        } yield Tell(Nil)
      },
      "(.*) is (.*)" -> {case List(x,y) =>
        for {
          xref <- referent(x)
          yref <- abstractReferent(y)
          _ <- fork(tell(Edge(xref,IsA,yref)))
        } yield Tell(Nil)
      }
    ))

  def abstractReferent(text: String): ForkedState[Context,Concept] =
    fork(List(Abstract(text)))
  
  def referent(text: String): ForkedState[Context,Concept] =
    for {
      what <- reifyingSearch1(Question(What,IsA,Abstract(text)))
      _ <- continueIf(what match {
        case Realized(_) => true
        case _ => false
      })
    } yield what

  def describe(response: Response): State[Context,String] =
    state(response.toString)

  def describe(concept: Concept, pos: NounPos): State[Context,String] =
    state(concept.toString)
}


class ConversationSuite extends FunSuite {
  
  val ctx = Context(MentalMap())
  val lang = new TestLanguage()
  val conv = Conversation(lang)

  test("Referring to dog should give the realized dog") {
    def run =
      for {
        dog <- fork(reify(Abstract("dog")))
        dogRef <- lang.referent("dog")
      } yield {
        assert(dogRef == dog)
      }
    run(ctx)
  }
  test("Asking for the abstractReferent of dog should give the abstract dog") {
    def run =
      for {
        dog <- fork(reify(Abstract("dog")))
        dogArche <- lang.abstractReferent("dog")
      } yield {
        assert(dogArche == Abstract("dog"))
      }
    run(ctx)
  }
  test("Parsing 'what is dog' should give the right question") {
    def run =
      for {
        dog <- fork(reify(Abstract("dog")))
        result <- lang.parse("What is dog?")
      } yield {
        assert(result == Tell(Edge(dog,IsA,Abstract("dog")) :: Nil))
      }
    run(ctx)
  }
  test("Asking 'what is dog' should return 'dog0 is dog','dog0 is animal'") {
    def run =
      for (dogResponse <- lang.parse("There is dog.");
           val Tell(List()) = dogResponse;
           dog <- lang.referent("dog");
           _ <- fork(conv.query("Dog is animal."));
           ask0 <- lang.parse("What is dog?"))
        yield {
          println(ask0)
          ask0 match {
            case Tell(edges) =>
              assert(edges.toSet == Set(Edge(dog,IsA,Abstract("dog")),
                  Edge(dog,IsA,Abstract("animal"))))
            case _ => assert(false)
          }
        }
    run(ctx)
  }
}
