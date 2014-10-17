import dreamer.concept._
import dreamer.conceptnet._
import Concept._
import Relation._


object Main {
  def main(args: Array[String]) = {
    val map = MentalMap(Some(new ConceptNet()))
    println(map.ask(Edge(What,IsA,Abstract("/c/en/toast"))))
    println(map.ask(Edge(Abstract("/c/en/toast"),IsA,What)))
  }
}
