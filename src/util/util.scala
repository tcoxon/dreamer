package util
import java.io._
import java.net._
import scala.util.parsing.json._

object Util {

  def uriEncode(text: String) = URLEncoder.encode(text, "UTF-8")

  def readStream(in0: InputStream) = {
    val in = new BufferedReader(new InputStreamReader(in0))
    try {
      val result = new StringBuilder()
      var inputLine: String = in.readLine()
      while (inputLine != null) {
        result.append(inputLine)
        result.append("\n")
        inputLine = in.readLine()
      }
      result.toString
    } finally {
      in.close()
    }
  }

  def fetchURL(url: URL): String = {
    debug("Fetching: "+url)
    readStream(url.openStream())
  }

  def fetchURLCached(url: URL): String = {
    val hash = url.toString.hashCode % 256
    val cacheFName = "dreamer-cache/urlhash"+hash.toString+".json";
    val file = new File(cacheFName)

    if (!file.getParentFile().exists) {
      val succ = !file.getParentFile.mkdirs()
      // TODO FIXME do some real error handling please
      assert(succ)
    }
    val json = if (file.exists) {
      val conts = readStream(new FileInputStream(file))
      JSON.parseFull(conts).asInstanceOf[Option[Map[String,String]]] match {
        case Some(x) => x
        case None => Map[String,String]()
      }
    } else {
      Map[String,String]()
    }

    val json1 = if (json.get(url.toString).isEmpty) {
      val r = json + (url.toString -> fetchURL(url))
      val fos = new FileOutputStream(file)
      val writer = new OutputStreamWriter(fos)
      try {
        writer.write(JSONObject(r).toString())
      } finally {
        writer.close()
      }
      r
    } else {
      debug("Reading cache: "+url.toString)
      json
    }

    val result = json1.get(url.toString)
    assert(!result.isEmpty)
    result.get
  }

  var hideDebug = false
  def debug(text: String) = if (hideDebug) () else println("debug: "+text)

}
