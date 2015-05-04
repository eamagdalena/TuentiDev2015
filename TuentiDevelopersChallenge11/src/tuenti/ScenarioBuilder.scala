package tuenti

/** Parses the scenario file and removes as most wrong paths possible */
class ScenarioBuilder {

  def buildScenario(file: Iterator[String], numScenario: Int): Scenario = {
    println("Building scenario " + numScenario + "...")

    val ss = file.next.split(" ")
    val stamina = ss.head.toInt
    val numberRooms = ss(1).toInt
    Scenario(numScenario, stamina, buildRooms(file, numberRooms, stamina))
  }

  def buildRooms(file: Iterator[String], numberRooms: Int, heroStamina: Int): List[Room] = {

    val roomsPhase1 = (for {
      numRoom <- 0 until numberRooms
      room = buildRoom(file, numRoom, heroStamina)
      if (isRoomLegal(room))
    } yield room).toList

    filterRooms(roomsPhase1, heroStamina)

  }

  def filterRooms(rooms: List[Room], heroStamina: Int) = {
    var numRoomsPreviousStep = rooms.size
    var roomsPhase2 = rooms

    /* PHASE 2 */

    do {

      println("Iterating Phase 2...")

      numRoomsPreviousStep = roomsPhase2.size

      /* Remove doors to nowhere */
      roomsPhase2 = roomsPhase2.map { r => Room(r.number, r.label, r.minions, r.doors.filter { x => doorIsLegal(x) }) }

      /* Remove rooms without doors */
      roomsPhase2 = roomsPhase2.filter(isRoomLegal(_))

    } while (roomsPhase2.size != numRoomsPreviousStep)

    /* PHASE 3 */

    do {

      println("Iterating Phase 3...")

      numRoomsPreviousStep = roomsPhase2.size

      /* Remove rooms that are impossible to exit due to stamina issues */
      var staminaMap = new MiniSimulator(roomsPhase2, heroStamina).miniSimulate
      roomsPhase2 = filterRoomsThatAreImpossibleToExit(roomsPhase2, staminaMap)

      /* Remove doors to nowhere */
      roomsPhase2 = roomsPhase2.map { r => Room(r.number, r.label, r.minions, r.doors.filter { x => doorIsLegal(x) }) }

      /* Remove rooms without doors */
      roomsPhase2 = roomsPhase2.filter(isRoomLegal(_))

    } while (roomsPhase2.size != numRoomsPreviousStep)

    roomsPhase2.toList

  }

  def isPossibleToLeave(room: Room, allDoors: List[(String, Door)], staminaMap: Map[String, Int]): Boolean = {
    if (room.label equals "start") return true

    val doorsThatBringHere = allDoors.filter(x => x._1 equals room.label)

    if (doorsThatBringHere.isEmpty) {
      println(room.label + "will be vanished because is impossible to reach")
      vanishedRooms = room.label :: vanishedRooms
      return false
    }

    //val minStaminaCostToArrive = doorsThatBringHere.minBy(_._2.stamina)._2.stamina
    val minStaminaCostToLeave = room.doors.minBy(_.stamina).stamina

    val legal = staminaMap(room.label) - minStaminaCostToLeave + room.minions >= 0

    if (!legal) {

      println(room.label + " will be vanished because is impossible to leave")
      vanishedRooms = room.label :: vanishedRooms
    }

    legal
  }

  def filterRoomsThatAreImpossibleToExit(rooms: List[Room], staminaMap: Map[String, Int]) = {

    val allDoors = rooms.flatMap { room => room.doors.map { door => (door.leadsTo, door) } }

    for {
      room <- rooms
      if (isPossibleToLeave(room, allDoors, staminaMap))
    } yield room

  }

  var vanishedRooms: List[String] = List.empty

  def isRoomLegal(room: Room) = {
    val legal = room.doors.size > 0 || (room.label equals "start")

    if (!legal) {

      println(room.label + " has been vanished!")
      vanishedRooms = room.label :: vanishedRooms
    }

    legal

  }

  def doorIsLegal(door: Door) = {
    val legal = !vanishedRooms.contains(door.leadsTo)

    if (!legal) {
      println("Door is not legal because room " + door.leadsTo + " vanished before")
    }

    legal
  }

  def doorIsLegal(door: Door, numberDoors: Int, heroStamina: Int) = {
    val legal = door.keys <= numberDoors && door.stamina <= heroStamina

    if (!legal) {
      //println("Door to " + door.leadsTo + " is ilegal by preconditions")
    }

    legal
  }

  def buildRoom(file: Iterator[String], numberRoom: Int, heroStamina: Int): Room = {

    val ss = file.next.split(" ")
    val label = ss.head
    val numberDoors = ss(1).toInt

    val doors = (for {
      numDoor <- 0 until numberDoors
      door = buildDoor(file, numDoor, heroStamina)
      if (doorIsLegal(door, numberDoors, heroStamina))
    } yield door).toList

    Room(numberRoom, label, numberDoors, doors)

  }

  def buildDoor(file: Iterator[String], numberDoor: Int, heroStamina: Int): Door = {
    val ss = file.next.split(" ")
    Door(numberDoor, ss.head, ss(1).toInt, ss(2).toInt)
  }

}
