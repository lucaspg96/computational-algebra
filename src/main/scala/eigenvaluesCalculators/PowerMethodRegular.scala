package eigenvaluesCalculators

import entities.{Matrix, Vector}
import helpers.VectorHelper

object PowerMethodRegular extends EigenvaluesCalculator {
  def calculate(A: Matrix, tolerance: Double = 0.001): (Double,Vector) = {
    if(!A.isSquare) throw new Error ("Matrix must be square!")

    var lambdaOld, lambda = 0.0
    var q = VectorHelper.createVector(A.shape._1,fill = 1).normalized

    do{
      lambdaOld = lambda
      q = (A*q).normalized
      lambda = (q*(A*q))/(q*q)
    }while(relativeError(lambda,lambdaOld) > tolerance)

    (lambda,q)
  }

  def relativeError(a: Double, b: Double):Double = math.abs(a-b)/a

  def main(args: Array[String]): Unit = {
    val A = new Matrix(3)

    A setRow (0,new Vector(1,2,3))
    A setRow (1,new Vector(4,5,6))
    A setRow (2,new Vector(7,8,10))

    val solution = 16.7075

    val (lambda,vector) = calculate(A)

    println(s"Expected: $solution")
    println(s"Result: $lambda")
    println(vector)
  }
}
