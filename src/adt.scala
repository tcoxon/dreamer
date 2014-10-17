package dreamer.adt
import scala.collection.immutable._


case class MultiMap[K,V](map: Map[K,Set[V]] = Map[K,Set[V]]()) {
  def +(kv: (K,V)) = {
    val (k,v) = kv
    this.copy(
      map = map + (k -> (map.get(k) match {
        case Some(values) => values + v
        case None => Set(v)
      })))
  }

  def -(kv: (K,V)) = {
    val (k,v) = kv
    map.get(k) match {
      case Some(xs) => this.copy(map = map + (k -> (xs - v)))
      case None => this
    }
  }

  def get(k: K): Set[V] = map.get(k) match {
    case Some(values) => values
    case None => Set()
  }
}

