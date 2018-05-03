package eigenvaluesCalculators

import entities.real.{Matrix, Vector}
import helpers.real.MatrixHelper

object PowerMethodDisplaced {
  def calculate(A: Matrix, tolerance: Double = 0.001, step: Double = 0.3): (List[Double], List[Vector]) = {
    val (m,n) = A.shape
    val disks = calculateDisks(A)
//    println(disks)

    var eigenvalues: List[Double] = List()
    var eigenvectors: List[Vector] = List()

    for{
      (center,radius) <- disks
      d <- (-radius) to radius by step
      if eigenvalues.lengthCompare(m) == -1
    } {
      val mi = center + d
      if (!A.diagonalAsVector.asList.exists(x => relativeError(x, mi) < tolerance)) {
        val (lambda, fi) = displacedFind(A, mi, tolerance / 1000)


        if (eigenvalues.forall(relativeError(lambda, _) > tolerance)) {
          eigenvalues +:= lambda
          eigenvectors +:= fi
        }
      }
    }

    (eigenvalues,eigenvectors)
  }

  def displacedFind(A: Matrix, mi: Double, tolerance: Double = 0.00001): (Double, Vector) = {
//    println(s"Deslocamento: $mi")
    val (m,n) = A.shape
    val A2 = A - (MatrixHelper.getIdentity(m,n)*mi)

    val (l,fi) = PowerMethodInverse.calculate(A2,tolerance)

    val lambda = l + mi
//    println(s"lambda: $lambda, fi: $fi")

    (lambda,fi)
  }

  def relativeError(a: Double, b: Double):Double = math.abs(a-b)

  def calculateDisks(A: Matrix): List[(Double,Double)] =
    (for(i <- 0 until A.shape._1) yield {
      val v =  A.rowAsVector(i)
      val r = v.asList.map(math.abs).sum - v(i)

      (v(i),r)
    }).toList

  def main(args: Array[String]): Unit = {
    val A = new Matrix(3)

    A setRow (0,new Vector(1,2,3))
    A setRow (1,new Vector(4,5,6))
    A setRow (2,new Vector(7,8,10))

    val (lambdas,vectors) = calculate(A)
    for((lambda,fi) <- lambdas.zip(vectors)){
      println(s"value: $lambda, vector: $fi")
    }

//    val (lambda,vector) = displacedFind(A, 6.700000000000001)
//    println(lambda)
//    println(vector)

  }
}
