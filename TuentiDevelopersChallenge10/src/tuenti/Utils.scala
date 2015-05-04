package tuenti

import scala.io.Source
import scala.reflect.io.Path

object Utils {

  def fromFile(path: String) = Source.fromFile(path).getLines

  def toFile(output: String, path: String) = Path(path).toFile.writeAll(output)

  def pretty(l: List[Any]) = l.mkString("\n")

  def pretty(s: String) = s

}
