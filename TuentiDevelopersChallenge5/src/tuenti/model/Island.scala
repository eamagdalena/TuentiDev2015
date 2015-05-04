package tuenti.model

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

class Island(val name: String, val cost: Int) {
  val routes: ArrayBuffer[(Island, Int)] = ArrayBuffer.empty

  override def toString = s"${name}(${cost})"
}

object Island {

  private val islands: HashMap[String, Island] = new HashMap

  lazy val treasureIsland: Island = islands("Raftel")

  def find(name: String): Island = islands(name)

  def apply(line: String): Island = {
    val ss = line.split(" ")
    val island = new Island(ss.head, ss.tail.head.toInt)
    islands.put(island.name, island)
    island
  }

}

case class Route(origin: Island, destiny: Island, cost: Int)

object Route {

  def apply(line: String): Route = {
    val ss = line.split(" ")
    val origin = Island.find(ss(0))
    val destiny = Island.find(ss(1))
    val cost = ss(2).toInt
    origin.routes += new Tuple2(destiny, cost)

    Route(origin, destiny, cost)
  }

}
