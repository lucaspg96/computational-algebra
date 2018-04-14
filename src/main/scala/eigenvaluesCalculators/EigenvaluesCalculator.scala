package eigenvaluesCalculators

import entities.Matrix

trait EigenvaluesCalculator {
  def calculate(A: Matrix, tolerance: Double = 0.001): Double
}
