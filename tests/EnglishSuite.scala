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
      "look", "Look around.", "look around this place!").map(
        msg => conv.query(msg)(ctx)._2)
    println(results)
    results foreach {r =>
      assert(r == "I am in a house. A dog is in the house.")
    }
  }
}
