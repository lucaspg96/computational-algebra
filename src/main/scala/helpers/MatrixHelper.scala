package helpers

import entities.Matrix

import scala.io.Source

object MatrixHelper {
  def loadFromFile(path: String, delimiter: String = ","): Matrix = {
    val file = Source.fromFile(path)

    val vectors = (for(line <- file.getLines()) yield VectorHelper.loadFromString(line)).toList

    if(vectors.isEmpty)
      new Matrix(0)

    if(vectors exists (v => v.length != vectors(0).length))
      throw new Error("Broken matrix")

    val result = new Matrix(vectors.length, vectors(0).length)

    for{
      i <- 0 until result.shape._1
      j <- 0 until result.shape._2
    } result.set((i,j),vectors(i)(j))

    result
  }
}
