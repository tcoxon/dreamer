import org.scalatest._
import dreamer.concept._

class MentalMapSuite extends FunSuite {
  import Concept._
  import Relation._

  val Test = Abstract("test")
  val map0 = MentalMap() + Edge(Self, IsA, Test)


  test("To 'What is Test' a test should answer Self") {
    assert(map0.ask(Question(What, IsA, Test)) == Set(Edge(Self, IsA, Test)))
  }
  test("Asking a test what Self is should answer Test") {
    assert(map0.ask(Question(Self, IsA, What)) == Set(Edge(Self, IsA, Test)))
  }
  test("""With no information about its location, asking where the test is
      should return no edges.""") {
    assert(map0.ask(Question(Self, AtLocation, What)) == Set())
  }
  test("Asking a question with two wildcards should report an exception") {
    intercept[IllegalArgumentException] {
      map0.ask(Question(What, IsA, What))
    }
  }
  test("Asking a true edge should yield the edge itself") {
    assert(map0.ask(Question(Self,IsA,Test)) == Set(Edge(Self,IsA,Test)))
  }
  test("Asking a false edge should yield the empty set") {
    assert(map0.ask(Question(Test,IsA,Self)) == Set())
  }

}
