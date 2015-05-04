package tuenti

import Utils._
import Endian._
import ByteUtils._
import Base64._

object Problem4 extends App {

  def convert(piece: Piece, bits: List[Char]): String = {

    val endianChunk = (piece.endian match {
      case BigEndian    => bits
      case LittleEndian => convertToLittleEndian(bits)
    })

    val chunk = if (piece.reverse) endianChunk.reverse else endianChunk

    BigInt(chunk.mkString, 2).toString

  }

  def solve(bits: List[Char], pieces: List[Piece]): List[String] =
    if (pieces.isEmpty) Nil
    else convert(pieces.head, bits.take(pieces.head.size)) :: solve(bits.drop(pieces.head.size), pieces.tail)

  val file = fromFile(args.head)

  val pieces = file.tail.tail.map(Piece(_))

  val cake = decodeBinary(file.head).iterator.toList.flatMap { convertBytetoBits(_) }

  val solution = solve(cake, pieces)

  println(solution)

  toFile(pretty(solution), s"${args.head}.solved")

}
