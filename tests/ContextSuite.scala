import org.scalatest._
import scalaz._, Scalaz._
import dreamer.concept._, Concept._, Relation._
import dreamer.context._, Context._


class ContextSuite extends FunSuite {

  val initCtx = Context(
        MentalMap()
          + Edge(Abstract("house"),AtLocation,Abstract("suburb"))
          + Edge(Abstract("house"),AtLocation,Abstract("street"))
          + Edge(Abstract("cat"),AtLocation,Abstract("house"))
          + Edge(Abstract("dog"),AtLocation,Abstract("house"))
          + Edge(Abstract("mountain"),AtLocation,Abstract("surface_of_earth"))
          + Edge(Abstract("mountain"),AtLocation,Abstract("mountainous_region"))
          + Edge(Abstract("goat"),AtLocation,Abstract("mountain"))
          + Edge(Abstract("tree"),AtLocation,Abstract("mountain"))
        )

  test("State monad should work for reification") {
    def run: State[Context,Unit] =
      for (house <- reify(Abstract("house"));
           mountain <- reify(Abstract("mountain"));
           a <- reifyingAsk(Question(What,AtLocation,house));
           b <- reifyingAsk(Question(house,AtLocation,What));
           c <- reifyingAsk(Question(mountain,AtLocation,What));
           d <- reifyingAsk(Question(What,AtLocation,mountain));
           e <- reifyingAsk(Question(house,AtLocation,mountain));
           f <- reifyingAsk(Question(Self,AtLocation,What));
           g <- reifyingAsk(Question(Self,IsA,What));
           ctx <- get)
        yield {
          val aSimple = a.map(edge => archetype(ctx, edge.start))
          val bSimple = b.map(edge => archetype(ctx, edge.end))
          val cSimple = c.map(edge => archetype(ctx, edge.end))
          val dSimple = d.map(edge => archetype(ctx, edge.start))
          assert((aSimple & Set(Abstract("cat"), Abstract("dog"))) != Set())
          assert((bSimple & Set(Abstract("suburb"), Abstract("street"))) !=
              Set())
          assert(b.size == 1)
          assert((cSimple & Set(Abstract("surface_of_earth"),
              Abstract("mountainous_region"))) != Set())
          assert(c.size == 1)
          assert((dSimple & Set(Abstract("goat"), Abstract("tree"))) != Set())
          assert(e.size == 0)
          assert(f.size == 0)
          assert(g.size == 0)
          ()
        }

    run(initCtx)
  }

}
