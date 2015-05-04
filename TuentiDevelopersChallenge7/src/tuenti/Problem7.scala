package tuenti

import Utils._

object Problem7 extends App {

  val input = fromFile(args.head).iterator

  val p = ProblemToSolve(input.next)

  val girls = for (i <- 1 to p.N) yield Girl(input.next)

  for (i <- 1 to p.M) Girl.connect(input.next.split(" ").map { g => Girl.find(g) }.toList)

  val res = girls.map { _.score }.max

  println(res)
  toFile(res.toString, s"${args.head}.solved")

}
