package linearSystemSolvers
import entities.{Matrix,Vector}
import math.sqrt

class CholeskySolver extends Solver{

  def getCholeskyMatrix(A: Matrix): Matrix = {

    if(!A.isSymmetric) throw new Error("Matrix is not symmetric!")

    val (m,n) = A.shape
    val cholesky: Matrix = new Matrix(m,n)

    for{
      i <- 0 until m
      j <- 0 until n
    }{
      if (j==0)
        if(i==0)
          if(A(0)(0) >= 0) cholesky set ((i,j), sqrt(A(0)(0)))
          else throw new Error("This matrix is not positive defined")
        else cholesky set ((i,j), A(i)(j)/cholesky(0)(0))

      else{
        val gij = (A(i)(j) - (for(k <- 1 until j) yield cholesky(i)(k)*cholesky(j)(k)).sum) / cholesky(j)(j)
        cholesky set ((i,j),gij)
      }

    }

    cholesky
  }

  def solve(A: Matrix, y: Vector): Vector = {

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
}
