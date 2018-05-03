package helpers.complex

import entities.complex.{Complex, ComplexVector}

object ComplexVectorHelper {
  def createVector(n: Int, fill: Complex = Complex(0,0)) =
    new ComplexVector(Array.fill[Complex](n)(fill):_*)
}
