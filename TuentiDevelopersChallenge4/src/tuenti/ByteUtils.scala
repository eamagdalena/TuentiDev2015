package tuenti

object ByteUtils {

  def convertBytetoBits(b: Byte) = {
    val bits = Integer.toBinaryString(b)

    if (bits.length() >= 8) bits.takeRight(8)
    else (for { i <- 0 to 7 - bits.length } yield '0').iterator.mkString + bits
  }

  private def bytedStream(bits: List[Char]): Stream[List[Char]] = {

    if (bits.isEmpty) Stream.Empty
    else bits.take(8) #:: bytedStream(bits.drop(8))

  }

  def convertToLittleEndian(bits: List[Char]) = {
    bytedStream(bits).toList.reverse.flatten

  }
}
