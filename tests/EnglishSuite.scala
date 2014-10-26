import org.scalatest._
import scalaz._, Scalaz._
import dreamer.concept._, Concept._, Relation._
import dreamer.context._, Context._
import dreamer.conversation._
import dreamer.lang.en._

class EnglishSuite extends FunSuite {
  def setupContext: State[Context,Unit] =
    for {
        house <- reify(Abstract("house"))
        cat <- reify(Abstract("cat"))
        dog <- reify(Abstract("dog"))
        garden <- reify(Abstract("garden"))
        street <- reify(Abstract("street"))
        _ <- tell(Edge(house,AtLocation,street))
        _ <- tell(Edge(garden,AtLocation,street))
        _ <- tell(Edge(cat,AtLocation,garden))
        _ <- tell(Edge(dog,AtLocation,house))
        _ <- tell(Edge(Self,AtLocation,house))
      } yield ()

  val conv = Conversation(new English)
  val ctx = setupContext(Context(MentalMap()))._1

  test("Looking should yield the dreamer's location and the dog.") {
    val results = List(
      "take a look", "Look around.", "look around this place!").map(
        msg => conv.query(msg)(ctx)._2)
    println(results)
    results foreach {r =>
      assert(r == "I am in a house. A dog is in the house.")
    }
  }
  test("Looking at the dog should explain what and where it is.") {
    def run: State[Context,Unit] =
      for {
        look <- conv.query("look")
        val _ = assert(look == "I am in a house. A dog is in the house.")
        result0 <- conv.query("Take a look at the dog.")
        result1 <- conv.query("Look at it.")
      } yield {
        println(look)
        println(result0)
        println(result1)
        assert(result0 == "It is a dog. It is in the house.")
        assert(result1 == "It is a dog. It is in the house.")
      }
    run(ctx)
  }
  test("""It's wrong, but refering to something that isn't there should
      currently return a ParseFailure.""") {
    def run: State[Context,Unit] =
      for {
        _ <- conv.query("look")
        result <- conv.query("look at the car")
      } yield {
        println(result)
        assert(result == "I don't understand.")
      }
    run(ctx)
  }
  test("Leaving the house should put the dreamer in the street") {
    def run: State[Context,Unit] =
      for {
        result <- conv.query("leave")
      } yield {
        println(result)
        assert(result == "I am in a street. A house is in the street. A garden is in the street.")
      }
    run(ctx)
  }
}
