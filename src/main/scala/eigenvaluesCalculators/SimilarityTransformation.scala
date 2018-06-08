package eigenvaluesCalculators

import entities.complex.{Complex, ComplexMatrix, ComplexVector}
import entities.real.{Matrix, Vector}
import helpers.complex.{ComplexMatrixHelper, ComplexVectorHelper}

import math.{abs, sqrt, pow}

object SimilarityTransformation {

  def calculate(A: ComplexMatrix, tolerance: Double = 0.001): (List[Complex], ComplexMatrix) = {
    val (ai,x) = householder(A)
    var Ai = ai
    var X = x
    println(s"A após householder:\n${Ai.toReal.toLatex}")
    var oldNoise, newNoise = 0.0
    do{
      oldNoise = newNoise
      val (q,r) = simplifiedJacobi(Ai)
      Ai = r * q
      X = X * q
//      println("Ai:")
//      println(Ai)
      newNoise = tridiagonalNoise(Ai)
//      println(newNoise)
    }while(math.abs(newNoise-oldNoise)>tolerance/10)

//    println(Ai)
    println("Ai:")
    println(Ai.toReal.toLatex)
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
//      println("Psi: ")
//      println(psi)
    }

    (lambdas,X*psi)
  }

  private def solveUH(A: ComplexMatrix, X: ComplexMatrix, tolerance: Double): (List[Complex], ComplexMatrix) = {
    var values: List[Complex] = List()

    var j = 0

    while(j<A.shape._2){
//      println(s"Checking subdiagonal $j element: ${A(j+1)(j)}")
      if(A(j+1)(j).real <= tolerance){
//        println("It is lte than tolerance")
        values +:= A(j)(j)
        j += 1
      }
      else{
//        println("It is NOT lte than tolerance")
        val trace = (A(j)(j)+A(j+1)(j+1)).real
//        println(s"trace: $trace")
        val det = (A(j)(j)*A(j+1)(j+1)).real - (A(j+1)(j).real * A(j)(j+1).real)
//        println(s"determinant: $det")
        val delta = pow(trace,2) - 4 * det

        val l1 = Complex(0,sqrt(-delta)/2) + trace/2
        val l2 = Complex(0,-sqrt(-delta)/2) + trace/2

        values ++= l1::(l2::Nil)
        j = j+2
      }
    }

    (values,new ComplexMatrix(A.shape._1, A.shape._2))
  }

  private def tridiagonalNoise(A: ComplexMatrix): Double =
    (for(j <- 0 until A.shape._2-1) yield math.abs(A(j+1)(j).real)).sum

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
    val A = new Matrix(5)

    //SM
    A setRow (0,new Vector(1,1,3,2,5))
    A setRow (1,new Vector(1,7,8,9,8))
    A setRow (2,new Vector(3,8,9,0,0))
    A setRow (3,new Vector(2,9,0,1,1))
    A setRow (4,new Vector(5,8,0,1,5))

//    //UT
//    A setRow (0,new Vector(1,1,3,2,5))
//    A setRow (1,new Vector(6,7,8,9,8))
//    A setRow (2,new Vector(2,3,3,0,0))
//    A setRow (3,new Vector(1,3,1,1,1))
//    A setRow (4,new Vector(2,1,0,0,5))

//    //UH
//    A setRow (0,new Vector(1,1,3,2,5))
//    A setRow (1,new Vector(6,7,8,9,10))
//    A setRow (2,new Vector(2,3,3,4,5))
//    A setRow (3,new Vector(1,3,1,1,1))
//    A setRow (4,new Vector(2,1,1,3,5))

    println(A.toLatex)
    val (lambdas,vectors) = calculate(A.toComplex)
    println(lambdas)
    println(vectors)
  }

}
