package helpers

import entities.{Matrix, MatrixValue}

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

  def getIdentity(m: Int,n: Int): Matrix = new Matrix(m,n) map {case MatrixValue(i,j,_) => if(i==j) 1.0 else 0.0}
  def getIdentity(n: Int): Matrix = getIdentity(n,n)
}
