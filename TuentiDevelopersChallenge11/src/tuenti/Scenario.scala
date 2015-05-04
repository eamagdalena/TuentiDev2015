package tuenti

case class Scenario(number: Int, startingStamina: Int, rooms: Seq[Room]) {

  val EXIT = "exit"

  override def toString = number + " Rooms: " + rooms.size

  lazy val roomsMap = rooms.map(room => (room.label -> room)).toMap

}

case class Room(number: Int, label: String, minions: Int, doors: Seq[Door])

case class Door(number: Int, leadsTo: String, keys: Int, stamina: Int)

object Scenario {

  def parseScenariosFile(file: Iterator[String], lastScenario: Int): List[Scenario] = {
    file.drop(1)
    (0 to lastScenario).map { i => new ScenarioBuilder().buildScenario(file, i) }.toList
  }

}
