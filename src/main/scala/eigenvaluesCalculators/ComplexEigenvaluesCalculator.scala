package eigenvaluesCalculators

import entities.complex.{Complex, ComplexMatrix, ComplexVector}
import entities.real.{Matrix, Vector}
import helpers.complex.{ComplexMatrixHelper, ComplexVectorHelper}

import math.{sqrt,abs}

object ComplexEigenvaluesCalculator {

  def calculate(A: ComplexMatrix, tolerance: Double = 0.001): (List[Complex], ComplexMatrix) = {
    val (ai,x) = householder(A)
    var Ai = ai
    var X = x

    do{
      val (q,r) = simplifiedJacobi(Ai)
      Ai = r * q
      X = X*q
    }while(tridiagonalNoise(Ai)>tolerance)

    if(A.isSymmetric) println("TD")
    else if (isUT(Ai, tolerance)) println("UT")
    else println("UH")
    (List[Complex](),Ai)
  }

  private def tridiagonalNoise(A: ComplexMatrix): Double =
    sqrt((for(j <- 0 until A.shape._2-1) yield !A(j+1)(j)).sum)

  private def isUT(A: ComplexMatrix, tolerance: Double): Boolean =
    (for{
      j <- 0 until A.shape._2-1
      if abs(A(j+1)(j).real) > tolerance
    } yield 0).isEmpty


  private def householder(A: ComplexMatrix): (ComplexMatrix, ComplexMatrix) = {
    val (m,n) = A.shape

    var Aj = A copy
    var H = ComplexMatrixHelper.getIdentity(m,n)

    for(j <- 0 until n-1){
      val Hj = getHj(Aj,j)
      Aj = Hj * Aj * Hj
      H = H*Hj
    }

    (Aj, H)
  }

  private def getHj(A: ComplexMatrix, j: Int): ComplexMatrix = {
    val Vj = ComplexVectorHelper.createVector(A.shape._2)
    for(k <- j+1 until A.shape._2) Vj set (k,A(j)(k))

    val V2 = ComplexVectorHelper.createVector(A.shape._2)
    val v = if(Vj(j+1) > Complex(0,0)) -Vj.norm else Vj.norm
    V2 set (j+1,v)

    val n = (Vj - V2).normalized

    ComplexMatrixHelper.getIdentity(A.shape._2) - (n.transpose*n.asMatrix)*2
  }

  private def simplifiedJacobi(A: ComplexMatrix): (ComplexMatrix, ComplexMatrix) = {
    val (m,n) = A.shape
    var Qt = ComplexMatrixHelper.getIdentity(m,n)
    var Aj = A copy

    for(j <- 0 until n-1){
      val Jjt = buildJijT(Aj,j+1,j)
      Aj = Jjt * Aj
      Qt = Jjt * Qt
    }

    (Qt.transpose, Aj)
  }

  def buildJijT(A: ComplexMatrix, i: Int, j: Int): ComplexMatrix = {
    val (m,n) = A.shape
    val J = ComplexMatrixHelper.getIdentity(m)

    val theta = if(A(j)(j)!= Complex(0,0)) math.atan(A(i)(j).real/A(j)(j).real) else math.Pi/2

    J set ((j,j),math.cos(theta))
    J set ((j,i),math.sin(theta))
    J set ((i,j),-math.sin(theta))
    J set ((i,i),math.cos(theta))

    J
  }

  def main(args: Array[String]): Unit = {
    val A = new Matrix(3)

    //UT
    A setRow (0,new Vector(1,2,3))
    A setRow (1,new Vector(4,5,6))
    A setRow (2,new Vector(7,8,10))

//    //TD
//    A setRow (0,new Vector(1,2,3))
//    A setRow (1,new Vector(2,5,6))
//    A setRow (2,new Vector(3,6,10))

    val (lambdas,vectors) = calculate(A.toComplex)

    println(vectors)
  }

}
