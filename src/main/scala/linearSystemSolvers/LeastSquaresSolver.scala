package linearSystemSolvers

import entities.real.{Matrix, Vector}

object LeastSquaresSolver extends Solver{

  def solve(A: Matrix, y: Vector): Vector = {
    LUSolver.solve(A.transpose*A, A.transpose*y)
  }

  def main(args: Array[String]): Unit = {
    val A: Matrix = new Matrix(3,2)
    A setRow (0, new Vector(1,-1))
    A setRow (1, new Vector(1, 1))
    A setRow (2, new Vector(2,1))

    val y = new Vector(2,4,8)

    val solution = new Vector(23.0/7.0, 8.0/7.0)

    println(A)
    val x = solve(A,y)
    println(s"Expected: $solution")
    println("\n"+x)
  }
}
