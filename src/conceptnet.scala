package dreamer.conceptnet
import java.net.URL
import scala.util.parsing.json.JSON

import dreamer.concept._
import Concept._
import Relation._
import dreamer.util.Util._


private object ConceptNet {
  val defaultBaseURL = "http://conceptnet5.media.mit.edu/data/5.2"
  val defaultTimeout = 20l

}

class ConceptNet(
    baseURL: String=ConceptNet.defaultBaseURL,
    timeout: Long=ConceptNet.defaultTimeout) extends EdgeSource {
  var memo = MentalMap()

  private def getConceptUri(c: Concept) = c match {
    case What => Some("-")
    case Abstract(uri) => Some(uri)
    case _ => None
  }
  private def getRelationUri(r: Relation) = r match {
    case IsA => Some("/r/IsA")
    case AtLocation => Some("/r/AtLocation")
  }
  private def getRelation(uri: String) = uri match {
    case "/r/IsA" => Some(IsA)
    case "/r/AtLocation" => Some(AtLocation)
    case _ => None
  }

  private def urlFor(start: String, rel: String, end: String): URL = {
    val search = baseURL + "/search?"
    val kvs = Array("start" -> start, "rel" -> rel, "end" -> end)
    new URL(search + (for ((k,v) <- kvs; if v != "-")
                        yield k+"="+uriEncode(v)).mkString("&"))
  }

  private def urlFor(e: Edge): Option[URL] = {
    val Edge(start, rel, end) = e

    val startUri = getConceptUri(start)
    val relUri = getRelationUri(rel)
    val endUri = getConceptUri(end)

    for (start <- startUri;
         rel <- relUri;
         end <- endUri)
      yield urlFor(start, rel, end)
  }

  private def parseResults(json: String): Set[Edge] =
    JSON.parseFull(json) match {
      case Some(obj: Map[String,List[Map[String,Any]]]) =>
        obj.get("edges") match {
          case Some(edges) =>
            (for (edge <- edges;
                  relStr <- edge.get("rel");
                  rel <- getRelation(relStr.toString);
                  start <- edge.get("start");
                  end <- edge.get("end"))
              yield Edge(Abstract(start.toString), rel,
                         Abstract(end.toString))).toSet
          case None => Set()
        }
      case _ => Set()
    }

  private def fetch(e: Edge): Set[Edge] = urlFor(e) match {
    case Some(url) => parseResults(fetchURL(url))
    case None => Set()
  }

  override def ask(e: Edge): Set[Edge] = {
    val cache = memo.ask(e)
    if (cache == Set()) {
      fetch(e) foreach (memo += _)
    }
    memo.ask(e)
  }

}
