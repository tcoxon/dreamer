package dreamer.conversation
import scalaz._, Scalaz._
import dreamer.concept._, Concept._, Relation._
import dreamer.context._, Context._
import dreamer.lang._, Language._
import dreamer.game._
import util.Util._


case class Conversation(lang: Language) {

  def query(text: String): State[Context,String] = {
    debug("Query: "+lang.normalizeInput(text))
    for {
      // TODO rather than parseSpecific, maybe choose one meaning based on
      // likelihood? Perhaps choose by edge score/weight and some bayesian math?
      response <- lang.parseSpecific(text)
      ctx:Context <- get
      _ <- clearIt
      simulationR <- Game.simulateWorld
      desc <- lang.describe(response + simulationR)
      val _ = debug("Query result: "+desc)
    } yield desc
  }

}

