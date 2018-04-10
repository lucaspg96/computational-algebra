package linearSystemSolvers

import entities.{Matrix, Vector}
import qr.{GramSchmidtOrtogonalization, HouseHolderOrtogonalization, JacobiOrtogonalization}

object QRSolver extends Solver
//    with GramSchmidtOrtogonalization
//  with HouseHolderOrtogonalization
  with JacobiOrtogonalization
{

  def solve(A: Matrix, y: Vector): Vector = {

    getQR(A)
//    println("---------------------")
//    println(Q)
//    println("---------------------")
//    println(R)
    GaussianEliminationSolver.setTotalPivote
    GaussianEliminationSolver.solve(R, Q.transpose * y)
  }

  def main(args: Array[String]): Unit = {
    val A: Matrix = new Matrix(3)
//    A set((0, 0), 1)
//    A set((0, 1), 1)
//    A set((0, 2), -1)
//
//    A set((1, 0), 1)
//    A set((1, 1), -2)
//    A set((1, 2), 5)
//
//    A set((2, 0), 4)
//    A set((2, 1), 1)
//    A set((2, 2), 4)
//
//    val solution = new Vector(2,3,5)
//
//    val y: Vector = new Vector(0, 21, 31)

    A set((0, 0), 3)
    A set((0, 1), 2)
    A set((0, 2), -1)

    A set((1, 0), 2)
    A set((1, 1), -2)
    A set((1, 2), 4)

    A set((2, 0), -1)
    A set((2, 1), 0.5)
    A set((2, 2), -1)

    val solution = new Vector(1,-2,-2)

    val y: Vector = new Vector(1, -2, 0)

    val x = solve(A, y)
    println("---------------------")
    println("Solution: " + x)
    println("Expected: " + solution)
  }
}
