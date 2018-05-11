package singularValueDecomposition

import eigenvaluesCalculators.ComplexEigenvaluesCalculator
import entities.complex.{Complex, ComplexMatrix}
import helpers.complex.ComplexMatrixHelper

import math.{sqrt}

object SVD {
  def calculate(A: ComplexMatrix, tolerance: Double = 0.0001): (ComplexMatrix, ComplexMatrix, ComplexMatrix) = {
    val (m, n) = A.shape

    val (lambdas, vectors) = ComplexEigenvaluesCalculator.calculate(A*A.transpose, tolerance)

    val S = ComplexMatrixHelper.getIdentity(m, n)
    for {
      i <- 0 until n
      val l = lambdas(i)
    }
      if (l.real >= 0) S set((i, i), Complex(sqrt(l.real), 0))
      else S set((i, i), Complex(sqrt(-l.real), 1))

    val U = vectors

    val V = ComplexEigenvaluesCalculator.calculate(A.transpose*A, tolerance)._2

    (U,S,V)
  }
}
