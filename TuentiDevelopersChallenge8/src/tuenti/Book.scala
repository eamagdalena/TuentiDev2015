package tuenti

import scala.collection.mutable.HashMap

class BookEntry(val name: String, val value: Long, val ingredients: List[BookEntry], var buildsInto: List[BookEntry] = Nil) {

  override def toString = s"${name} ${value} Ingredients: ${ingredients.map { _.name }} Builds into: ${buildsInto.map { _.name }}"

  lazy val distinctIngredients = ingredients.toSet

  lazy val distinctIngredientsWithCounts = distinctIngredients.map { x => (x, ingredients.count { x == _ }) }

}

object BookEntry {

  def apply(line: String): BookEntry = {

    val ss = line.split(" ")

    val entry = new BookEntry(ss.head, ss(1).toLong, ss.drop(2).map { find(_) }.toList)

    entry.ingredients.foreach { x => x.buildsInto = entry :: x.buildsInto }

    entries.put(entry.name, entry)

    entry

  }

  private val entries: HashMap[String, BookEntry] = new HashMap

  def find(name: String): BookEntry = entries(name)

}
