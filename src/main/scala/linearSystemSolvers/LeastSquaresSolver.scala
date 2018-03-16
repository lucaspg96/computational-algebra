package linearSystemSolvers

import entities.{Matrix,Vector}

object LeastSquaresSolver extends Solver{

  def solve(A: Matrix, y: Vector): Vector = {
    GaussianEliminationSolver.setTotalPivote
    GaussianEliminationSolver.solve(A.transpose*A, A.transpose*y)
  }

  def main(args: Array[String]): Unit = {
    val A: Matrix = new Matrix(3)
    A set ((0,0),1)
    A set ((0,1),1)
    A set ((0,2),-1)

    A set ((1,0),1)
    A set ((1,1),-2)
    A set ((1,2),5)

    A set ((2,0),4)
    A set ((2,1),1)
    A set ((2,2),4)

    val y: Vector = new Vector(0, 21, 31)

    val x = solve(A,y)
    println("\n"+x)
  }
}
