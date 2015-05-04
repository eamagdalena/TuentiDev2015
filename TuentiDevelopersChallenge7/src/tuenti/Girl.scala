package tuenti

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

class Girl(val name: String, val likeNaughtyGames: Boolean, val likeSuperHeroes: Boolean, val likeLeisureMen: Boolean,
           val likeCats: Boolean, val likeShopping: Boolean) {

  override def toString = name

  val friends: ArrayBuffer[Girl] = ArrayBuffer.empty

  def addFriend(girl: Girl) = if (girl != this && !friends.contains(girl)) friends += girl

  lazy val score: Int = {

    val suitScore = 6 * friends.iterator
      .flatMap { _.friends.iterator }
      .toSet
      .count { h => h.likeLeisureMen && h != this && !friends.contains(h) }

    val catLoverFriend = friends.find { h => h.likeCats && !h.friends.exists { j => j.likeCats && j != this } } != None

    val res = (if (likeNaughtyGames) 7 else 0) +
      (3 * friends.count { _.likeSuperHeroes }) +
      suitScore +
      (if (catLoverFriend) 4 else 0) +
      5 * notConnectedWhoLoveShopping.size

    //    println(this + " score summary:")
    //    println("7 points if G likes naughty, dirty games: " + (if (likeNaughtyGames) 7 else 0))
    //    println("3 points for every friend of G who likes super hero action figures: " + (3 * friends.count { _.likeSuperHeroes }))
    //    println("6 points for every friend of a friend of G, not including the friends of G and G herself, who likes men in leisure suits: " + suitScore)
    //    println("4 points if G has any friend H who likes cats, and no friend of H (except perhaps G) likes cats (4 points at most, not 4 for every friend) : " + (if (catLoverFriend) 4 else 0))
    //    println("5 points for every girl H who likes to go shopping and has no possible connection with G through a chain of friends (friends, friends of friends, friends of friends of friends, etc.): " + 5 * notConnectedWhoLoveShopping.size)
    //    println("FRIENDS: " + friends)
    //    println("NOT CONNECTED: " + notConnected)
    //    println("NOT CONNECTED WHO SHOPS: " + notConnectedWhoLoveShopping)
    //    println("TOTAL POINTS : " + res)

    res

  }

  private lazy val notConnected: Set[Girl] = {

    def discoverNetwork(girl: Girl, girlsNotDiscovered: Set[Girl]): Set[Girl] = {

      var result = girlsNotDiscovered

      for (g <- girl.friends.iterator) {
        if (result.contains(g)) result = discoverNetwork(g, result - g)
      }

      result
    }

    discoverNetwork(this, Girl.girls.values.toSet - this)

  }

  private lazy val notConnectedWhoLoveShopping: Set[Girl] = notConnected.filter { _.likeShopping }

}

object Girl {

  private val girls: HashMap[String, Girl] = new HashMap

  def find(name: String): Girl = girls(name)

  def apply(line: String): Girl = {
    val ss = line.split(" ")
    val girl = new Girl(ss.head, ss(1).equals("Y"), ss(2).equals("Y"), ss(3).equals("Y"), ss(4).equals("Y"), ss(5).equals("Y"))
    girls.put(girl.name, girl)

    girl
  }

  def connect(friends: List[Girl]) {

    if (friends.size > 1) {

      val head = friends.head

      friends.tail.foreach { girl =>
        girl.addFriend(head)
        head.addFriend(girl)
      }

      connect(friends.tail)
    }
  }

}
