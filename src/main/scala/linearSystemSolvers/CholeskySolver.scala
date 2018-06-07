package linearSystemSolvers
import entities.real.{Matrix, Vector}

import math.sqrt

object CholeskySolver extends Solver{

  def getCholeskyMatrix(A: Matrix): Matrix = {

    if(!A.isSymmetric) throw new Error("Matrix is not symmetric!")

    val (m,n) = A.shape
    val cholesky: Matrix = new Matrix(m,n)

    for{
      i <- 0 until m
      j <- 0 to i
    }{
      if(i==j) { //diagonal
        if (i == 0) //first
          if (A(0)(0) >= 0) cholesky set((i, j), sqrt(A(0)(0)))
          else throw new Error("This matrix is not positive defined")

        else {
          val gii = A(i)(i) - (for (k <- 0 until i) yield cholesky(i)(k) * cholesky(i)(k)).sum
          if (gii < 0) throw new Error("This matrix is not positive defined")
          else cholesky set((i, i), sqrt(gii))
        }
      }
      else{ //lower triangle
        if(j == 0) cholesky set ((i,0),A(i)(0)/cholesky(0)(0)) //first column
        else { //other columns
          val gij = (A(i)(j) - (for (k <- 0 until j) yield cholesky(i)(k) * cholesky(j)(k)).sum) / cholesky(j)(j)
          cholesky set ((i,j),gij)
        }
      }

    }

    cholesky
  }

  def solve(A: Matrix, y: Vector): Vector = {
    val cholesky = getCholeskyMatrix(A)

    println("A:")
    println(A.toLatex)

    println("Cholesky matrix:")
    println(cholesky.toLatex)

    val z = FirstSolver(cholesky,y)

    SecondSolver(cholesky transpose, z)
  }

  def FirstSolver(A: Matrix,y: Vector): Vector = {
    val (m,n) = A.shape
    var solution = List[Double]()
    for(i <- 0 until m){
      val row = A.rowAsVector(i)
      //      println(row+" = "+y(i))

      val v = (for(k <- 0 until i) yield {
        //        print(solution(k)+"*"+row(k)+" ")
        solution(k)*row(k)
      }).sum //replacing already known values and summing them
      //      println()
      //      println(row(i)+"x + "+v+" = "+y(i))
      //      println(row(i)+"x = "+(y(i)-v))
      val xi = (y(i) - v)/row(i)
      //      println("x = "+xi)
      //      println()
      solution = solution :+ xi
    }

    new Vector(solution:_*)
  }

  def SecondSolver(A: Matrix,y: Vector): Vector = {
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
    A set ((0,0),25)
    A set ((0,1),15)
    A set ((0,2),-5)

    A set ((1,0),15)
    A set ((1,1),18)
    A set ((1,2),0)

    A set ((2,0),-5)
    A set ((2,1),0)
    A set ((2,2),11)

    val y: Vector = new Vector(-2, 0.25, 10.2)

    val x = solve(A,y)
    println("\n"+x)
  }
}
