package linearSystemSolvers.iteractive

import entities.real.{Matrix, Vector}
import helpers.real.VectorHelper
import linearSystemSolvers.Solver

object GaussSeidelIteractiveSolver extends Solver {

  override def solve(A: Matrix, y: Vector): Vector = {
    val tolerance = 0.001
    var oldX = VectorHelper.createVector(A.shape._2)
    var x: Vector = VectorHelper.createVector(A.shape._2)
    var i = 1
    do{
      oldX = x.copy

      for(i <- 0 until x.length) yield {
        //(y(k) -(A.rowAsVector(k)*x) + A(k)(k)*x(k))/A(k)(k)
        val s: Double = (for(j <- 0 until x.length; if j!=i) yield A(i)(j)*x(j)).sum
        x.set(i,(y(i) - s)/A(i)(i))
      }

      println(s"x$i: $x")
      i += 1
    }while(vectorError(x,oldX) > tolerance)

    x

  }

  private def vectorError(a: Vector, b: Vector): Double =
    (a-b).map{case(_,v) => math.abs(v)}.asList.sum


  def main(args: Array[String]): Unit = {
    val A: Matrix = new Matrix(3)
    A set((0, 0), 8)
    A set((0, 1), 1)
    A set((0, 2), -1)

    A set((1, 0), 1)
    A set((1, 1), -7)
    A set((1, 2), 2)

    A set((2, 0), 2)
    A set((2, 1), 1)
    A set((2, 2), 9)

    val solution = new Vector(1,1,1)

    val y: Vector = new Vector(8,-4,12)

    val x = solve(A, y)
    println("---------------------")
    println("Solution: " + x)
    println("Expected: " + solution)
  }

}
