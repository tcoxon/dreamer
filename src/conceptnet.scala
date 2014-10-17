package dreamer.conceptnet
import scala.collection.mutable.Map
import dreamer.concept._
import Concept._
import Relation._
import dreamer.util.Util._


private object ConceptNet {
  val defaultBaseURL = "http://conceptnet5.media.mit.edu/data/5.2"
}

class ConceptNet(baseURL: String=ConceptNet.defaultBaseURL) extends EdgeSource {
  val memo: Map[Edge, Set[Edge]] = Map()

  private def getConceptUri(c: Concept) = c match {
    case What => Some("-")
    case Abstract(uri) => Some(uri)
    case _ => None
  }
  private def getRelationUri(r: Relation) = r match {
    case IsA => Some("/r/IsA")
    case AtLocation => Some("/r/AtLocation")
  }

  private def fetch(start: String, rel: String, end: String): Set[Edge] = {
    val search = baseURL + "/search?"
    val kvs = Array("start" -> start, "rel" -> rel, "end" -> end)
    val url = search + (for ((k,v) <- kvs; if v != "-")
                          yield k+"="+uriEncode(v)).mkString("&")
    debug("Fetching "+url)
    Set[Edge]()
  }

  private def fetch(e: Edge): Set[Edge] = {
    val Edge(start, rel, end) = e

    val startUri = getConceptUri(start)
    val relUri = getRelationUri(rel)
    val endUri = getConceptUri(end)

    (for (start <- startUri;
         rel <- relUri;
         end <- endUri)
      yield fetch(start, rel, end)) match {
      case Some(result) => result
      case None => Set[Edge]()
    }
  }

  override def ask(e: Edge): Set[Edge] = memo.get(e) match {
    case Some(result) => result
    case None =>
      val result = fetch(e)
      memo += (e -> result)
      result
  }

}
