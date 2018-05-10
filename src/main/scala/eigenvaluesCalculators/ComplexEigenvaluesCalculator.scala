package eigenvaluesCalculators

import entities.complex.{Complex, ComplexMatrix, ComplexVector}
import entities.real.{Matrix, Vector}
import helpers.complex.{ComplexMatrixHelper, ComplexVectorHelper}
import org.scalactic.Tolerance

import math.{abs, sqrt, pow}

object ComplexEigenvaluesCalculator {

  def calculate(A: ComplexMatrix, tolerance: Double = 0.000001): (List[Complex], ComplexMatrix) = {
    val (ai,x) = householder(A)
    var Ai = ai
    var X = x
//    println(s"A após householder:\n$Ai")
    do{
      val (q,r) = simplifiedJacobi(Ai)
      Ai = r * q
      X = X * q
    }while(tridiagonalNoise(Ai)>tolerance)

//    println(Ai)

    if(A.isSymmetric) solveTD(Ai,X)
    else if (isUT(Ai, tolerance)) solveUT(Ai,X)
    else solveUH(Ai,X,tolerance)

  }

  private def solveTD(A: ComplexMatrix, X: ComplexMatrix): (List[Complex], ComplexMatrix) =
    (A.diagonalAsVector.asList, X)

  private def solveUT(A: ComplexMatrix, X: ComplexMatrix): (List[Complex], ComplexMatrix) = {
    val (m,n) = A.shape
    val lambdas = A.diagonalAsVector.asList
    val psi = ComplexMatrixHelper.getIdentity(m,n)

    for{
      i <- 1 until m
      j <- i-1 to 0 by -1
    }{
      val s = (for(k <- j+1 to i-1 by -1) yield A(k)(j)*psi(k)(i))
        .foldLeft(Complex(0,0))((acc,c) => acc+c)
      val xi = (-A(j)(i) - s)/(A(j)(j) - A(i)(i))
      psi set ((j,i),xi)
      println("Psi: ")
      println(psi)
    }

    (lambdas,X*psi)
  }

  private def solveUH(A: ComplexMatrix, X: ComplexMatrix, tolerance: Double): (List[Complex], ComplexMatrix) = {
    var values: List[Complex] = List()

    var j = 0

    while(j<A.shape._2){
      if(A(j+1)(j).real <= tolerance){
        values +:= A(j+1)(j)
        j += 1
      }
      else{
        val trace = (A(j)(j)*A(j+1)(j+1)).real
        val det = trace - (A(j+1)(j) * A(j)(j+1)).real
        val delta = pow(trace,2) - 4 * det

        val l1 = (Complex(sqrt(-delta),1)/2) + trace/2
        val l2 = -(Complex(sqrt(-delta),1)/2) + trace/2

        values ++= l1::(l2::Nil)
        j = j+2
      }
    }

    (values,X)
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
      Aj = Hj * (Aj * Hj)
      H = H*Hj
    }

    (Aj, H)
  }

  private def getHj(A: ComplexMatrix, j: Int): ComplexMatrix = {
    val Vj = ComplexVectorHelper.createVector(A.shape._2)
    for(k <- j+1 until A.shape._2) Vj set (k,A(k)(j))

    val V2 = ComplexVectorHelper.createVector(A.shape._2)
    val v = if(Vj(j+1).real > 0) -Vj.norm else Vj.norm
    V2 set (j+1,Complex(v,0))

    val n = (Vj - V2).normalized
//    println("Vj-V2:")
//    println(Vj-V2)
//    println("Normalizing:")
//    println(n)
//    println("----------------------")

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
//    println(s"Aj:\n$Aj")
    (Qt.transpose, Aj)
  }

  def buildJijT(A: ComplexMatrix, i: Int, j: Int): ComplexMatrix = {
    val (m,n) = A.shape
    val Jt = ComplexMatrixHelper.getIdentity(m)

    val theta = if(A(j)(j).real != 0) math.atan(A(i)(j).real / A(j)(j).real) else math.Pi/2

    Jt set ((j,j),math.cos(theta))
    Jt set ((j,i),math.sin(theta))
    Jt set ((i,j),-math.sin(theta))
    Jt set ((i,i),math.cos(theta))

    Jt
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
    println(lambdas)
    println(vectors)
  }

}
