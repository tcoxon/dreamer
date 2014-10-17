package dreamer.util
import java.net.URLEncoder

object Util {

  def uriEncode(text: String) = URLEncoder.encode(text, "UTF-8")


  def debug(text: String) = println("debug: "+text)

}
