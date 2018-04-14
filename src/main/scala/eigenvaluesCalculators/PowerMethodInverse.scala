package eigenvaluesCalculators

import entities.{Matrix, Vector}
import helpers.VectorHelper
import linearSystemSolvers.{CholeskySolver, GaussianEliminationSolver, LUSolver, QRSolver}

object PowerMethodInverse extends EigenvaluesCalculator {
  def calculate(A: Matrix, tolerance: Double = 0.001): Double = {
    if(!A.isSquare) throw new Error ("Matrix must be square!")

    var lambdaOld, lambda = 0.0
    var q = VectorHelper.createVector(A.shape._1)
    q set (1,1)

    do{
      lambdaOld = lambda
      val x = LUSolver.solve(A,q)
//      println(s"x: $x")
      q = x.normalized
//      println(s"q: $q")
      lambda = (q*(A*q))/(q*q)
//      println(s"lambda: $lambda")
    }while(relativeError(lambda,lambdaOld) > tolerance)

    lambda
  }

  def relativeError(a: Double, b: Double):Double = math.abs(a-b)/a

  def main(args: Array[String]): Unit = {
    val A = new Matrix(3)

    A setRow (0,new Vector(1,2,3))
    A setRow (1,new Vector(4,5,6))
    A setRow (2,new Vector(7,8,10))

    val solution = 0.1982

    val lambda = calculate(A)

    println(s"Expected: $solution")
    println(s"Result: $lambda")
  }
}
