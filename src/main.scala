import dreamer.concept._
import dreamer.conceptnet._
import Concept._
import Relation._


object Main {
  def main(args: Array[String]) {
    val map = MentalMap(Some(new ConceptNet())) + Edge(Self,IsA,Unknown)

    println("Hi")
  }
}
