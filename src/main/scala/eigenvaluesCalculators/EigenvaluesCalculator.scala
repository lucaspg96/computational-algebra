package eigenvaluesCalculators

import entities.real.{Matrix, Vector}

trait EigenvaluesCalculator {
  def calculate(A: Matrix, tolerance: Double = 0.001): (Double, Vector)
}
