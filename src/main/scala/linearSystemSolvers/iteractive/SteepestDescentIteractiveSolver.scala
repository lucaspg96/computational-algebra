package linearSystemSolvers.iteractive

import entities.real.{Matrix, Vector}
import helpers.real.VectorHelper
import linearSystemSolvers.Solver

object SteepestDescentIteractiveSolver extends Solver {

  private def gradient(A: Matrix, x: Vector, b: Vector): Vector =
    (A * x) - b

  override def solve(A: Matrix, y: Vector): Vector = {
    val tolerance = 0.001
    var xk = VectorHelper.createVector(A.shape._2)
    var k = 0

    do {
      val Rxk = gradient(A, xk, y)
      println(s"Rx$k = $Rxk")
      println(s"Rx$k norm = ${Rxk.norm}")

      val alpha: Double = (Rxk * Rxk) / (Rxk * (A * Rxk))
      println(s"alpha: $alpha")
      println(s"Rx$k * alpha = ${Rxk * alpha}")

      k += 1

      xk = xk - (Rxk * alpha)
      println(s"x$k = $xk")

    } while (gradient(A, xk, y).norm > tolerance)

    xk
  }

  def main(args: Array[String]): Unit = {
//    val A: Matrix = new Matrix(3)
//    A set((0, 0), 8)
//    A set((0, 1), 1)
//    A set((0, 2), -1)
//
//    A set((1, 0), 1)
//    A set((1, 1), -7)
//    A set((1, 2), 2)
//
//    A set((2, 0), 2)
//    A set((2, 1), 1)
//    A set((2, 2), 9)
//
//    val solution = new Vector(1, 1, 1)
//
//    val y: Vector = new Vector(8, -4, 12)

    val A: Matrix = new Matrix(2)
    A setRow (0, new Vector(1,1))
    A setRow (1, new Vector(2,3))

    val solution = new Vector(1, 2)

    val y: Vector = new Vector(3,8)

    val x = solve(A, y)
    println("---------------------")
    println("Solution: " + x)
    println("Expected: " + solution)

  }
}
