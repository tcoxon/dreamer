import scalaz._, Scalaz._
import dreamer.concept._, Concept._, Relation._
import dreamer.conceptnet._
import dreamer.context._, Context._
import dreamer.conversation._
import dreamer.lang.en._
import util.Util._

object CLI {

  def simpleHouseGame: Context = {
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

    setupContext(Context(MentalMap()))._1
  }

  def conceptNetGame: Context = {
    Context(MentalMap(upstream=Some(new ConceptNet)))
  }

  val conv = Conversation(new English)
  
  def repl(line: String, ctx: Context) {
    val (ctx1, response) = conv.query(line)(ctx)
    println(response)
    println()
    val nextLine = readLine("> ")
    if (nextLine != null) {
      repl(nextLine, ctx1)
    }
  }

  def main(args: Array[String]) {
    //hideDebug = true
    //repl("look",initialContext)
    repl("go to sleep. look",conceptNetGame)
  }
}
