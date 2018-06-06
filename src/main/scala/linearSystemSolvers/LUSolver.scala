package linearSystemSolvers
import entities.real.{Matrix,Vector}
import helpers.real.MatrixHelper

object LUSolver extends Solver{
  var currentDecomposedMatrix: Matrix = MatrixHelper.getIdentity(2,2)
  var L,U = MatrixHelper.getIdentity(2,2)

  def solve(A: Matrix, y: Vector): Vector = {

    if(!(A==currentDecomposedMatrix)){
//      println("Nova matriz")
      val (l,u) = getLU(A)
      L = l
      U = u
      currentDecomposedMatrix = A copy
    }
    println(L)
    println()
//    println(y)
    val z = LSolver(L,y)

//    println()

    println(U)
    println()
    println(z)
    USolver(U,z)

  }

  def LSolver(A: Matrix,y: Vector): Vector = {
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

  def USolver(A: Matrix,y: Vector): Vector = {
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

  def getLU(A: Matrix): (Matrix,Matrix) = {
    val (m,n) = A.shape

    var U = A copy
    val L = MatrixHelper. getIdentity(m,n)

    for{
      i <- 0 until n //row
      j <- i+1 until n //following row
    }{

      if(U(i)(i)==0) throw new Error("Can't decompose matrix to LU")

      var baseRow = U.rowAsVector(i) //row i

      val targetRow = U.rowAsVector(j) //row j
      val alpha = -U(j)(i)/U(i)(i) //signal is inverted
      L set ((j,i),-alpha)

      val newRow = (baseRow*alpha)+targetRow
      U.setRow(j,newRow)
    }

    (L,U)
  }

  def main(args: Array[String]): Unit = {
    val A: Matrix = new Matrix(3)
    A set ((0,0),2)
    A set ((0,1),1)
    A set ((0,2),-1)

    A set ((1,0),-3)
    A set ((1,1),-1)
    A set ((1,2),2)

    A set ((2,0),-2)
    A set ((2,1),1)
    A set ((2,2),2)

    val y: Vector = new Vector(8, -11, -3)

    val x = solve(A,y)
    println("\n"+x)
  }
}
