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
  val defaultMinWeight = 1.0
  val defaultMaxResults = 50

  val specials = Question(What,IsA,Thing) ::
                 Question(What,IsA,Place) :: Nil
  val specialMax = 500

  val bannedWords =
        // offensive
        "prostitute" ::
        "hooker" ::
        "period" ::
        "breast" ::
        "jew" ::
        "nigger" ::
        "gay person" ::
        "sex" ::
        // way too generic
        "something" :: "someone" :: "area" ::
        "there" :: "this" :: "it" ::
        Nil

  val badPrefixes =
        "the " :: "a " :: "any " :: "your " :: "my " :: "on " ::
        "in " :: "at " ::
        "sex " ::
        "one " :: "two " :: "three " :: "four " :: "five " ::
        "six " :: "seven " :: "eight " :: "nine " :: "ten " ::
        (0 until 9).map(_.toString).toList

  def filter(edges: List[Map[String,Any]]): List[Map[String,Any]] = for {
    e <- edges
    val start = e.get("startLemmas").asInstanceOf[Option[String]]
    if start.isEmpty || (!bannedWords.contains(start.get) &&
        badPrefixes.find(start.get.startsWith(_)).isEmpty)
    val end = e.get("endLemmas").asInstanceOf[Option[String]]
    if end.isEmpty || (!bannedWords.contains(end.get) &&
        badPrefixes.find(end.get.startsWith(_)).isEmpty)
  } yield e


  def builtins: (Set[Edge],Set[(String,Concept)]) = (Set(
      // Augment conceptnet with a few more builtin concepts
      Edge(DreamerGame,IsA,Abstract("/c/en/computer_game")),
      Edge(Abstract("/c/en/conceptnet"),IsA,Abstract("/c/en/database"))
    ), Set(
      "dreamer of electric sheep" -> DreamerGame,
      "conceptnet" -> Abstract("/c/en/conceptnet"),
      "looming" -> Abstract("/c/en/looming"),
      "grabbing" -> Abstract("/c/en/grabbing"),
      "laughing maniacally" -> Abstract("/c/en/laughing_maniacally"),
      "yelling \"WAKE UP\"" -> Abstract("/c/en/yelling_wake_up")
    ))
}

class ConceptNet(
    baseURL: String=ConceptNet.defaultBaseURL,
    timeout: Long=ConceptNet.defaultTimeout,
    minWeight: Double=ConceptNet.defaultMinWeight,
    maxResults: Int=ConceptNet.defaultMaxResults)
    extends EdgeSource {
  type Self = ConceptNet

  private var memo: MentalMap = {
    val (edges, names) = ConceptNet.builtins
    names.foldLeft(edges.foldLeft(MentalMap())(_+_)){ (mind, name_concept) =>
      val (name,concept) = name_concept
      mind.name(concept, name)
    }
  }
  private var alreadyFetched = Set[URL]()

  def nameOf(c: Concept): Option[String] = memo.nameOf(c) orElse (c match {
    case Abstract(uri) =>
      val url = new URL(baseURL + uri)
      fetch(url) foreach (memo += _)
      memo.nameOf(c)
    case _ => None
  })
  def named(name: String): Option[Concept] = memo.named(name) orElse {
    val url = new URL(baseURL + "/search?text=" + uriEncode(name))
    fetch(url) foreach (memo += _)
    memo.named(name)
  }
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
    case Verb(_) => None
    case NextTo(_) => None
    case HasState => None
  }
  private def getRelation(uri: String) = uri match {
    case "/r/IsA" => Some(IsA)
    case "/r/AtLocation" => Some(AtLocation)
    case "/r/HasA" => Some(HasA)
    case _ => None
  }

  private def urlFor(start: String, rel: String, end: String, max: Int): URL = {
    val search = baseURL + "/search?" +
        "minWeight=" + minWeight.toString +
        "&limit=" + max.toString + "&"
    val kvs = Array("start" -> start, "rel" -> rel, "end" -> end)
    new URL(search + (for ((k,v) <- kvs; if v != "-")
                        yield k+"="+uriEncode(v)).mkString("&"))
  }

  private def urlFor[T](q: Question[T]): Option[URL] = {
    val Question(start, rel, end) = q

    val startUri = getConceptUri(start)
    val relUri = getRelationUri(rel)
    val endUri = getConceptUri(end)

    val max = if (ConceptNet.specials contains q) ConceptNet.specialMax
              else maxResults

    for (start <- startUri;
         rel <- relUri;
         end <- endUri)
      yield urlFor(start, rel, end, max)
  }

  private def fetch[T](url: URL): Set[Edge] = {
    if (alreadyFetched contains url) Set()
    else {
      val r = parseResults(fetchURLCached(url))
      alreadyFetched += url 
      r
    }
  }

  private def fetch[T](q: Question[T]): Set[Edge] = urlFor(q) match {
    case Some(url) => fetch(url)
    case None => Set()
  }

  // The following functions are gross impure monsters.
  private def parseResults[T](json: String): Set[Edge] =
    JSON.parseFull(json) match {
      case Some(obj) =>
        obj.asInstanceOf[Map[String,List[Map[String,Any]]]].get("edges") match {
          case Some(edges0) =>
            val edges = ConceptNet.filter(edges0)
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
            val parsedEdges = (for (edge <- edges;
                  relStr <- edge.get("rel");
                  rel <- getRelation(relStr.toString);
                  start <- edge.get("start");
                  end <- edge.get("end"))
              yield Edge(Abstract(start.toString), rel,
                         Abstract(end.toString)))
            parsedEdges.filter(
                _ match {
                  case Edge(x,_,y) => x != y
                  case _ => true
                }).toSet
          case None => Set()
        }
      case _ => Set()
    }

  override def ask[T](q: Question[T]): Set[Edge] = {
    fetch(q) foreach (memo += _)
    memo.ask(q)
  }

}
