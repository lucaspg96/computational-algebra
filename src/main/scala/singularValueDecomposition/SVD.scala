package singularValueDecomposition

import eigenvaluesCalculators.SimilarityTransformation
import entities.complex.{Complex, ComplexMatrix}
import entities.real.{Matrix, Vector}
import helpers.complex.ComplexMatrixHelper

import math.sqrt

class SVD(A: ComplexMatrix, tolerance: Double = 0.0001) {

  def this(A: Matrix) = this(A.toComplex)

  val (m, n) = A.shape

  val (lambdas, vectors) = SimilarityTransformation.calculate(A * A.transpose, tolerance)

  lazy val S: ComplexMatrix = ComplexMatrixHelper.getIdentity(m, n)
  for {
    i <- 0 until n
    val l = lambdas(i)
  }
    if (l.real >= 0) S set((i, i), Complex(sqrt(l.real), 0))
    else S set((i, i), Complex(sqrt(-l.real), 1))

  lazy val U: ComplexMatrix = vectors

  lazy val V: ComplexMatrix = SimilarityTransformation.calculate(A.transpose * A, tolerance)._2

}

object RunSVD extends App {
  val A = new Matrix(3)

  A setRow (0,new Vector(1,2,3))
  A setRow (1,new Vector(4,5,6))
  A setRow (2,new Vector(7,8,10))

  val svd = new SVD(A)

  println("U:")
  println(svd.U.toReal.toLatex)
  println("-----------")
  println("S:")
  println(svd.S.toReal.toLatex)
  println("-----------")
  println("V:")
  println(svd.V.toReal.toLatex)
  println("-----------")
}
