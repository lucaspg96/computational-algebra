package helpers.complex

import entities.complex.{Complex, ComplexMatrix, ComplexMatrixValue}

object ComplexMatrixHelper {
  def getIdentity(m: Int,n: Int): ComplexMatrix =
    new ComplexMatrix(m,n) map {
    case ComplexMatrixValue(i,j,_) => if(i==j) Complex(1,0) else Complex(0,0)
    }

  def getIdentity(n: Int): ComplexMatrix = getIdentity(n,n)
}
