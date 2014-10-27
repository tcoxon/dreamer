package dreamer.conversation
import scalaz._, Scalaz._
import dreamer.concept._, Concept._, Relation._
import dreamer.context._, Context._
import dreamer.lang._, Language._


case class Conversation(lang: Language) {

  def query(text: String): State[Context,String] =
    for {
      response <- lang.parseSpecific(text)
      _ <- clearIt
      desc <- lang.describe(response)
    } yield desc

}

