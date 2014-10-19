import org.scalatest._
import scalaz._, Scalaz._
import dreamer.concept._, Concept._, Relation._
import dreamer.context._, Context._
import dreamer.conversation._

class TestLanguage extends Language {
  def abstractReferent(text: String) =
    for (refs <- referent(text); ctx: Context <- get)
      yield refs.map(ref => archetype(ctx, ref))
  // TODO
}
