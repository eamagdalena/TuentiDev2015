package tuenti

import Utils._

object Problem1 extends App {

  def solve(num: Double): Long = {
    Math.ceil(num / 2).toLong
  }

  val result = for (line <- fromFile(args.head).drop(1)) yield solve(line.toLong).toString

  toFile(pretty(result), s"${args.head}.solved")

}
