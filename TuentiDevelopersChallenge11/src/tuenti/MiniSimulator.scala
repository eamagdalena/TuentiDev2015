package tuenti

class MiniSimulator(val rooms: List[Room], val initialStamina: Int) {

  def miniSimulate = {

    val allDoors = rooms.flatMap { room => room.doors.map { door => (room, door.leadsTo, door) } }

    var staminaMap = Map[String, Int]().updated("start", initialStamina)

    def calculateStamina(room: Room): Int = {

      println("Calculating stamina for room " + room.label + "...")

      val doorsThatBringHere = allDoors.filter(x => x._2 equals room.label)

      if (doorsThatBringHere.isEmpty) return -1

      val maxStaminaHere = (for {
        originDoor <- doorsThatBringHere
        staminaOrigin = staminaMap.getOrElse(originDoor._1.label, calculateStamina(originDoor._1))
        door = originDoor._3
        minionsToKill = Math.max(door.stamina - staminaOrigin, door.keys)
        staminaAfterKills = Math.min(initialStamina, staminaOrigin + minionsToKill)
        staminaAfterDoor = Math.min(initialStamina, staminaAfterKills - door.stamina)
      } yield {

        staminaAfterDoor

      }).max

      staminaMap = staminaMap.updated(room.label, maxStaminaHere)

      println("Stamina for room " + room.label + " is " + maxStaminaHere)

      maxStaminaHere

    }

    println("Mini simulating staminas...")

    for {
      room <- rooms.filter { r => !(r.label equals "start") }
      if (!staminaMap.contains(room.label))
    } calculateStamina(room)

    println(staminaMap.toList)

    staminaMap
  }

}
