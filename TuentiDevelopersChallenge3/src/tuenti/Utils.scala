package tuenti

import scala.io.Source
import scala.reflect.io.Path

object Utils {

  def fromFile(path: String): List[String] = Source.fromFile(path).getLines.toList

  def toFile(output: String, path: String) = Path(path).toFile.writeAll(output)

  def solveSimple(inputPath: String, f: String => String) {
    toFile(f(fromFile(inputPath).head), inputPath + ".solved.txt")
  }

  def solveSimplePretty(inputPath: String, f: String => List[Any]) {
    toFile((f andThen pretty)(fromFile(inputPath).head), inputPath + ".solved.txt")
  }

  def solveMultiple(inputPath: String, f: List[String] => String) {
    val v = f(fromFile(inputPath))
    println(v)
    toFile(v, inputPath + ".solved.txt")
  }

  def solveMultiplePretty(inputPath: String, f: List[String] => List[Any]) {
    val v = (f andThen pretty)(fromFile(inputPath))
    println(v)
    toFile(v, inputPath + ".solved.txt")
  }

  def pretty(l: List[Any]) = l.mkString("\n")

  def pretty(s: String) = s

}
