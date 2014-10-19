import org.scalatest._
import dreamer.conceptnet._
import dreamer.concept._


class ConceptNetSuite extends FunSuite {
  import Concept._
  import Relation._

  val cn = new ConceptNet()

  test("Toast should be a kind of bread") {
    assert(cn.ask(Question(Abstract("/c/en/toast"),IsA,What)) contains
      Edge(Abstract("/c/en/toast"),IsA,Abstract("/c/en/bread")))
  }

  test("""Searching for animals that lives in a houses should yield 
        'cat' and 'dog'""") {
    val results = cn.search(
      Question(What, IsA, Abstract("/c/en/animal")),
      Question(What, AtLocation, Abstract("/c/en/house")))
    println(results)
    assert(results.contains(Map(() -> Abstract("/c/en/cat"))))
    assert(results.contains(Map(() -> Abstract("/c/en/dog"))))
  }
}
