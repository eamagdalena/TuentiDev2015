package tuenti

/* Fantastic immutable state part 2 */

trait Action

/** Bussiness Actions */
case class Open(door: Door) extends Action

case class DecideToGoAfter(door: Door) extends Action /* Signals the target and kills the creep */

case class Masacre(minionsToKill: Int) extends Action

case class Run(
  stamina: Int,
  keys: Int,
  location: Room,
  doorToOpen: Door,
  doorToOpenIsBlocked: Boolean,
  minionsLeftInTheRoom: Int,
  deadSet: List[BigInt],
  reachedExit: Boolean,
  scene: Scenario) {

  def decideToAimForDoor(door: Door) = Run(
    if (stamina < scene.startingStamina) stamina + 1 else scene.startingStamina,
    1,
    location,
    door,
    false,
    minionsLeftInTheRoom - 1,
    BigInt(1) :: deadSet,
    false,
    scene)

  def masacre(minionsToKill: Int) =
    Run(
      if (stamina + minionsToKill < scene.startingStamina) stamina + minionsToKill else scene.startingStamina,
      keys + minionsToKill,
      location,
      doorToOpen,
      false,
      minionsLeftInTheRoom - minionsToKill,
      Run.combinator(minionsLeftInTheRoom, minionsToKill) :: deadSet,
      false,
      scene)

  def openDoor(door: Door) = {
    val newLocation = scene.roomsMap.getOrElse(door.leadsTo, null)

    if (newLocation != null) {
      Run(stamina - door.stamina, 0, newLocation, null, true, newLocation.minions, deadSet, false, scene)
    } else { /* EXIT */
      Run(stamina - door.stamina, 0, null, null, true, 0, deadSet, door.leadsTo equals scene.EXIT, scene)
    }

  }

  def doAction(action: Action): Run = {

    action match {
      case Open(door: Door)            => openDoor(door)
      case DecideToGoAfter(door: Door) => decideToAimForDoor(door)
      case Masacre(minionsToKill: Int) => masacre(minionsToKill)
    }
  }

  def exited = reachedExit

  def possibleActions: Vector[Action] = { /* The magical function which makes this work */

    if (doorToOpen == null) { /* We need to decide among all the potential doors to open */

      location.doors.filter { door => door.stamina <= stamina + location.minions }.map { door => DecideToGoAfter(door) }.toVector

    } else if (stamina >= doorToOpen.stamina && keys >= doorToOpen.keys) {
      Vector(Open(doorToOpen))
    } else { /* Time for masacre */

      val masacreSize = Math.max(doorToOpen.stamina - stamina, doorToOpen.keys - keys)

      Vector(Masacre(masacreSize))
    }

  }

}

object Run {

  import CachedFactorial._

  def apply(scenario: Scenario): Run = {
    val start = scenario.roomsMap("start")
    Run(scenario.startingStamina, 0, start, null, true, start.minions, List.empty, false, scenario)
  }

  def combinator(minionsToKill: Int, masacreSize: Int): BigInt =
    factorial(minionsToKill) / (factorial(masacreSize) * factorial(minionsToKill - masacreSize))

}
