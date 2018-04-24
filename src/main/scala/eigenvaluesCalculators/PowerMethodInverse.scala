package eigenvaluesCalculators

import entities.{Matrix, Vector}
import helpers.VectorHelper
import linearSystemSolvers.{GaussianEliminationSolver, LUSolver}

object PowerMethodInverse extends EigenvaluesCalculator {
  def calculate(A: Matrix, tolerance: Double = 0.001): (Double, Vector) = {
    if(!A.isSquare) throw new Error ("Matrix must be square!")

    var lambdaOld, lambda = Double.MinValue
    var q = VectorHelper.createVector(A.shape._1,fill = 1).normalized
    GaussianEliminationSolver.setNoPivote
    do{
      lambdaOld = lambda
      val x = LUSolver.solve(A,q)
//      val x = GaussianEliminationSolver.solve(A,q)
//      println(s"x: $x")
      q = x.normalized
//      println(s"q: $q")
//      println(s"q: $q")
      lambda = (q*(A*q))/(q*q)
//      println(s"lambda: $lambda")
    }while(relativeError(lambda,lambdaOld) > tolerance)

    (lambda,q)
  }

  def relativeError(a: Double, b: Double):Double = math.abs(a-b)

  def main(args: Array[String]): Unit = {
    val A = new Matrix(3)

    A setRow (0,new Vector(1,2,3))
    A setRow (1,new Vector(4,5,6))
    A setRow (2,new Vector(7,8,10))

    val solution = 0.1982

    val (lambda, vector) = calculate(A)

    println(s"Expected: $solution")
    println(s"Result: $lambda")

    println(vector)
  }
}
