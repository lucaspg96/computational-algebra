package linearSystemSolvers

import entities.real.{Matrix, Vector}
import qr.{GramSchmidtOrtogonalization, HouseHolderOrtogonalization, JacobiOrtogonalization}

object QRSolver extends Solver
//    with GramSchmidtOrtogonalization
  with HouseHolderOrtogonalization
//  with JacobiOrtogonalization
{

  def solve(A: Matrix, y: Vector): Vector = {

    getQR(A)
    println("\nQ:")
    println(Q)
    println("\nR:")
    println(R)

    Substitution(R, Q.transpose * y)
  }

  def Substitution(A: Matrix,y: Vector): Vector = {
    val (m,n) = A.shape
    var solution = List[Double]()
    for(i <- m-1 to 0 by -1){
      val row = A.rowAsVector(i)
      //      println(row+" = "+auxiliar_y(i))

      val v = (for(k <- n-1 until i by -1) yield {
        //        print(solution((n-1)-(k))+"*"+row(k)+" ")
        solution((n-1)-(k))*row(k)
      }).sum //replacing already known values and summing them
      //      println()
      //      println(row(i)+"x + "+v+" = "+auxiliar_y(i))
      //      println(row(i)+"x = "+(auxiliar_y(i)-v))
      val xi = (y(i) - v)/row(i)
      //      println("x = "+xi)
      //      println()
      solution = solution :+ xi
    }

    new Vector(solution.reverse:_*)
  }

  def main(args: Array[String]): Unit = {
    val A: Matrix = new Matrix(3)
    A set((0, 0), 1)
    A set((0, 1), 1)
    A set((0, 2), -1)

    A set((1, 0), 1)
    A set((1, 1), -2)
    A set((1, 2), 5)

    A set((2, 0), 4)
    A set((2, 1), 1)
    A set((2, 2), 4)

    val solution = new Vector(2,3,5)

    val y: Vector = new Vector(0, 21, 31)

//    A set((0, 0), 3)
//    A set((0, 1), 2)
//    A set((0, 2), -1)
//
//    A set((1, 0), 2)
//    A set((1, 1), -2)
//    A set((1, 2), 4)
//
//    A set((2, 0), -1)
//    A set((2, 1), 0.5)
//    A set((2, 2), -1)
//
//    val solution = new Vector(1,-2,-2)
//
//    val y: Vector = new Vector(1, -2, 0)

    val x = solve(A, y)
    println("---------------------")
    println("Solution: " + x)
    println("Expected: " + solution)
  }
}
