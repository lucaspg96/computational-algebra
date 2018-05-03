package helpers.real

import entities.real.Vector

import scala.io.Source

object VectorHelper {
  def createVector(n: Int, fill: Double = 0) = new Vector(Array.fill[Double](n)(fill):_*)

  def loadFromFile(path: String, delimiter: String = ","): Vector = {
    val file = Source.fromFile(path)

    loadFromString(file.getLines().take(1).toList(0), delimiter)
  }

  def loadFromString(line: String, delimiter: String = ","): Vector = {
    val numbers = for(v <- line.split(delimiter)) yield v.toDouble

    new Vector(numbers:_*)
  }
}
