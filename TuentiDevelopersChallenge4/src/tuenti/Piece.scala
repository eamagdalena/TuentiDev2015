package tuenti

import Endian._

case class Piece(size: Int, endian: Endian, reverse: Boolean)

object Piece {
  def apply(s: String): Piece =
    if (s.last != 'R')
      Piece(s.dropRight(1).toInt, Endian(s.last), false)
    else
      Piece(s.dropRight(2).toInt, Endian(s.dropRight(1).last), true)
}
