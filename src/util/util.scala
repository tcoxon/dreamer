package util
import java.io._
import java.net._

object Util {

  def uriEncode(text: String) = URLEncoder.encode(text, "UTF-8")

  def fetchURL(url: URL): String = {
    debug("Fetching "+url)
    val in = new BufferedReader(new InputStreamReader(url.openStream()))
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

  def debug(text: String) = println("debug: "+text)

}
