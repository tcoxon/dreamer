package appletui
import java.awt.image._
import org.gba.spritely._
import GameUtil._
import util.Util._


object Sprites {
  
  private var sprites = Map[String,Option[BufferedImage]]()


  def requestImage(query: String)(cb: (String,BufferedImage)=>Unit) {
    if (sprites contains query) {
      val maybeImage = sprites(query)
      if (!maybeImage.isEmpty) cb(query, maybeImage.get)
      else cb(query, null)
    } else {
      // Stop repeated calls while fetch is in progress:
      sprites += query -> None
      fetchImage(query)(cb)
    }
  }

  private def fetchImage(query: String)(cb: (String,BufferedImage)=>Unit) = {
    debug("Spawning spritely fetch")
    spawn("Spritely search: "+query) {
      val spritely = new Spritely()
      spritely.setImagesPerSource(1)
      spritely.setSize(1)
      spritely.setSearchOpenClipart(true)
      spritely.setQuery(uriEncode(query))
      val results = spritely.search()
      if (results.size > 0) {
        debug("Found "+results.size+" results for "+query)
        val image = results.get(0)
        sprites += query -> Some(image)
        cb(query, image)
      } else {
        debug("No images found for "+query)
        sprites += query -> None
        cb(query, null)
      }
    }
  }

}


class SpritePanel extends ImagePanel((1,1), null) {

  var fallback: String = null

  private var _query: String = null
  def query = _query
  def query_=(q: String) { this.synchronized {
    this._query = q
    this.image = null

    if (q != null)
    Sprites.requestImage(q) {(q, img) =>
      swingThread { this.synchronized {
        if (q == this._query) {
          if (img == null && this.fallback != q) {
            this.query = this.fallback
          } else {
            if (img != null)
              this.imageSize = chooseSize(img)
            this.image = img
          }
        }
      }}
    }
  }}

  private def chooseSize(img: BufferedImage): (Int,Int) = {
    val (w,h) = (img.getWidth, img.getHeight)

    if (w < 300 && h < 300) {
      // Increase the max dimension until it's 300px
      if (w > h) {
        (300, h * 300/w)
      } else {
        (w * 300/h, 300)
      }

    } else if (w > getWidth || h > getHeight) {
      // Scale it down until it fits in the image
      val dw = (w+0.0)/getWidth
      val dh = (h+0.0)/getHeight
      if (dw > dh) {
        (getWidth, (h/dw).toInt)
      } else {
        ((w/dh).toInt, getHeight)
      }
    } else {
      // It fits!
      (w,h)
    }
  }

}
