import org.scalatest._
import dreamer.conceptnet._
import dreamer.concept._


class ConceptNetSuite extends FunSuite {
  import Concept._
  import Relation._

  val cn = new ConceptNet()

  test("Toast should be a kind of bread") {
    assert(cn.ask(Edge(Abstract("/c/en/toast"),IsA,What)) contains
      Edge(Abstract("/c/en/toast"),IsA,Abstract("/c/en/bread")))
  }
}
