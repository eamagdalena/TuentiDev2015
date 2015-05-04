package tuenti

trait Endian

case object LittleEndian extends Endian
case object BigEndian extends Endian

object Endian {
  def apply(c: Char): Endian = {
    c match {
      case 'L' => LittleEndian
      case 'B' => BigEndian
    }
  }
}
