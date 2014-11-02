package dreamer.conceptnet
import java.net.URL
import scala.util.parsing.json.JSON

import dreamer.concept._
import Concept._
import Relation._
import util.Util._


private object ConceptNet {
  val defaultBaseURL = "http://conceptnet5.media.mit.edu/data/5.2"
  val defaultTimeout = 20l
  val defaultMinWeight = 2.0
  val defaultMaxResults = 50
}

class ConceptNet(
    baseURL: String=ConceptNet.defaultBaseURL,
    timeout: Long=ConceptNet.defaultTimeout,
    minWeight: Double=ConceptNet.defaultMinWeight,
    maxResults: Int=ConceptNet.defaultMaxResults)
    extends EdgeSource {
  type Self = ConceptNet

  private var memo = MentalMap()

  def nameOf(c: Concept): Option[String] = memo.nameOf(c)
  def named(name: String): Option[Concept] = memo.named(name)
  def name(c: Concept, name: String): ConceptNet = {
    // yuck impure
    memo = memo.name(c, name)
    this
  }

  private def getConceptUri[T](c: QFragment[T]) = c match {
    case Variable(_) => Some("-")
    case Abstract(uri) => Some(uri)
    case _ => None
  }
  private def getRelationUri(r: Relation) = r match {
    case IsA => Some("/r/IsA")
    case AtLocation => Some("/r/AtLocation")
    case HasA => Some("/r/HasA")
    case PastAction(_) => None
  }
  private def getRelation(uri: String) = uri match {
    case "/r/IsA" => Some(IsA)
    case "/r/AtLocation" => Some(AtLocation)
    case "/r/HasA" => Some(HasA)
    case _ => None
  }

  private def urlFor(start: String, rel: String, end: String): URL = {
    val search = baseURL + "/search?" +
        "minWeight=" + minWeight.toString +
        "&limit=" + maxResults.toString + "&"
    val kvs = Array("start" -> start, "rel" -> rel, "end" -> end)
    new URL(search + (for ((k,v) <- kvs; if v != "-")
                        yield k+"="+uriEncode(v)).mkString("&"))
  }

  private def urlFor[T](q: Question[T]): Option[URL] = {
    val Question(start, rel, end) = q

    val startUri = getConceptUri(start)
    val relUri = getRelationUri(rel)
    val endUri = getConceptUri(end)

    for (start <- startUri;
         rel <- relUri;
         end <- endUri)
      yield urlFor(start, rel, end)
  }

  private def fragmentMatches[T](qf: QFragment[T], uri: String) = qf match {
    case Abstract(x) => x == uri
    case Variable(_) => true
    case _ => false
  }

  private def fetch[T](q: Question[T]): Set[Edge] = urlFor(q) match {
    case Some(url) => parseResults(q, fetchURL(url))
    case None => Set()
  }

  // The following functions are gross impure monsters.
  private def parseResults[T](q: Question[T], json: String): Set[Edge] =
    JSON.parseFull(json) match {
      case Some(obj) =>
        obj.asInstanceOf[Map[String,List[Map[String,Any]]]].get("edges") match {
          case Some(edges) =>
            def nameNode(nodeKey: String, lemmaKey: String) {
              for (edge <- edges;
                   node <- edge.get(nodeKey).asInstanceOf[Option[String]];
                   lemma <- edge.get(lemmaKey).asInstanceOf[Option[String]]) {
                if (memo.named(lemma).isEmpty ||
                    memo.nameOf(Abstract(node)).isEmpty)
                  memo = memo.name(Abstract(node), lemma)
              }
            }
            nameNode("start", "startLemmas")
            nameNode("end", "endLemmas")
            (for (edge <- edges;
                  relStr <- edge.get("rel");
                  rel <- getRelation(relStr.toString);
                  start <- edge.get("start");
                  end <- edge.get("end");
                  if fragmentMatches(q.start, start.toString) &&
                      fragmentMatches(q.end, end.toString))
              yield Edge(Abstract(start.toString), rel,
                         Abstract(end.toString))).toSet
          case None => Set()
        }
      case _ => Set()
    }

  override def ask[T](q: Question[T]): Set[Edge] = {
    val cache = memo.ask(q)
    if (cache == Set()) {
      fetch(q) foreach (memo += _)
    }
    memo.ask(q)
  }

}
