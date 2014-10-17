import org.scalatest._

class MentalMapSuite extends FunSuite {
  import Concept._
  import Relation._

  val map0 = MentalMap() + Edge(Self, IsA, Unknown)


  test("With itself unknown, 'What is Unknown' should answer Self") {
    assert(map0.ask(Edge(What, IsA, Unknown)) == Set(Edge(Self, IsA, Unknown)))
  }
  test("With itself unknown, asking what Self is should answer Unknown") {
    assert(map0.ask(Edge(Self, IsA, What)) == Set(Edge(Self, IsA, Unknown)))
  }
  test("""With no information about its location, asking where it is should
      return no edges.""") {
    assert(map0.ask(Edge(Self, AtLocation, What)) == Set())
  }
  test("Asking a question with two wildcards should report an exception") {
    intercept[IllegalArgumentException] {
      map0.ask(Edge(What, IsA, What))
    }
  }
  test("Asking a true edge should yield the edge itself") {
    assert(map0.ask(Edge(Self,IsA,Unknown)) == Set(Edge(Self,IsA,Unknown)))
  }
  test("Asking a false edge should yield the empty set") {
    assert(map0.ask(Edge(Unknown,IsA,Self)) == Set())
  }

}
