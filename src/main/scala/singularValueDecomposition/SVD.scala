package singularValueDecomposition

import eigenvaluesCalculators.ComplexEigenvaluesCalculator
import entities.complex.{Complex, ComplexMatrix}
import entities.real.Matrix
import helpers.complex.ComplexMatrixHelper

import math.sqrt

class SVD(A: ComplexMatrix, tolerance: Double = 0.0001) {

  def this(A: Matrix) = this(A.toComplex)

  val (m, n) = A.shape

  val (lambdas, vectors) = ComplexEigenvaluesCalculator.calculate(A * A.transpose, tolerance)

  lazy val S: ComplexMatrix = ComplexMatrixHelper.getIdentity(m, n)
  for {
    i <- 0 until n
    val l = lambdas(i)
  }
    if (l.real >= 0) S set((i, i), Complex(sqrt(l.real), 0))
    else S set((i, i), Complex(sqrt(-l.real), 1))

  lazy val U: ComplexMatrix = vectors

  lazy val V: ComplexMatrix = ComplexEigenvaluesCalculator.calculate(A.transpose * A, tolerance)._2


}
