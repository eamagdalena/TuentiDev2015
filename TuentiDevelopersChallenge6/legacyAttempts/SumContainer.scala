package tuenti

class SumContainer(var sum: Int, val x: Int, val y: Int) {

  def +=(i: Int) {
    sum += i
  }

  override def toString = sum.toString
}
