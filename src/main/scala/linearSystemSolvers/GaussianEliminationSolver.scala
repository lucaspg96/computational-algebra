package linearSystemSolvers
import entities.{Matrix, MatrixValue, Vector}

object GaussianEliminationSolver extends Solver {
  var pivoteType: Int = 1

  def setNoPivote = pivoteType = 0
  def setPartialPivote = pivoteType = 1
  def setTotalPivote = pivoteType = 2

  def solve(A: Matrix, y: entities.Vector): Vector = gaussianElimination(A,y)

  private def partialPivote(A: Matrix, y: Vector,k: Int): (Matrix, Vector) = {
    var swap = k

    for(i <- k+1 until A.shape._1) {
      if (A(i)(i) > A(swap)(swap)) {
        swap = i
      }
    }

    if(swap == k)
      (A,y)

    else {
      //println("Swapping line "+k+" with "+swap)
      val newMatrix = A map { case MatrixValue(i, j, v) =>
        if (i == k) A(swap)(j) else if (i == swap) A(k)(j) else v
      }

      val newVector = y map { case (i, v) =>
        if (i == k) y(swap) else if (i == swap) y(k) else v
      }

      (newMatrix, newVector)
    }


  }

  private def totalPivote(A: Matrix,y: Vector, indexes: List[Int],k: Int, l: Int): (Matrix, Vector, List[Int]) = {
    val (m,n) = A.shape
    var swap = (k,l)

    for{
      i <- k+1 until m
      j <- l+1 until n
    } {
      if (A (i)(j) > A (swap._1)(swap._2)) {
        swap = (i,j)
      }
    }


    //println("Swapping line "+k+" with "+swap._1)
    val partialMatrix = A map {case MatrixValue(i,j,v) =>
      if(i==k) A(swap._1)(j) else if(i==swap._1) A(k)(j) else v
    }

    val newVector = y map {case (i,v) =>
      if(i==k) y(swap._1) else if(i==swap._1) y(k) else v
    }
//    println()
//    println(partialMatrix)
//    println()
//    println(newVector)

    //println("Swapping column "+l+" with "+swap._2)
    val totalMatrix = partialMatrix map {case MatrixValue(i,j,v) =>
      if(j==l) partialMatrix(i)(swap._2) else if(j==swap._2) partialMatrix(i)(l) else v
    }
//    println()
//    println(totalMatrix)

    //there is no column change
    if(swap._2 == l)
      (totalMatrix,newVector,indexes)
    else{
      val newIndexes = for(i <- indexes) yield if(i==l) indexes(swap._2) else if(i==swap._2) indexes(l) else i
      (totalMatrix,newVector,newIndexes)
    }
  }

  def gaussianElimination(A: Matrix, y: Vector): Vector = {
    val (m,n) = A.shape

    if(m!=n) throw new Error("Only square matrices can have defined solution!")
    if(n!=y.length) throw new Error("Dimensions not match")

    var auxiliar_matrix = A copy
    var auxiliar_y = y copy
    var indexes = (0 until n).toList

//    println()
//    println(auxiliar_matrix)
//    println()
//    println(auxiliar_y)

    for{
      i <- 0 until n //row
      j <- i+1 until n //following row
    }{
      if(i<m-1) {
        if(pivoteType==1) {
          val (matrix,vector) = partialPivote(auxiliar_matrix,auxiliar_y,i)
          auxiliar_matrix = matrix
          auxiliar_y = vector
        }
        if(pivoteType==2){
          val (matrix,vector,ind) = totalPivote(auxiliar_matrix,auxiliar_y,indexes,i,j)
          auxiliar_matrix = matrix
          auxiliar_y = vector
          indexes = ind
        }

//        if(pivoteType!=0){
//          println()
//          println(auxiliar_matrix)
//          println()
//          println(auxiliar_y)
//        }
      }

      var baseRow = auxiliar_matrix.rowAsVector(i) //row i

      val targetRow = auxiliar_matrix.rowAsVector(j) //row j
      val alpha = -auxiliar_matrix(j)(i)/auxiliar_matrix(i)(i) //signal is inverted

      val newRow = (baseRow*alpha)+targetRow
      auxiliar_matrix.setRow(j,newRow)

      val yv = auxiliar_y(i)*alpha + auxiliar_y(j)
      auxiliar_y set(j,yv)

//      println()
//      println(auxiliar_matrix)
//      println()
//      println(auxiliar_y)

    }

//    println()
//    println(auxiliar_matrix)
//    println()
//    println(auxiliar_y)

    var solution = List[Double]()
    for(i <- m-1 to 0 by -1){
      val row = auxiliar_matrix.rowAsVector(i)
//      println(row+" = "+auxiliar_y(i))
      if(row(i)==0)
        if(auxiliar_y(i)==0) throw new Error("Multiple solutions found")
      else throw new Error("This system has no solution")

      val v = (for(k <- n-1 until i by -1) yield {
//        print(solution((n-1)-(k))+"*"+row(k)+" ")
        solution((n-1)-(k))*row(k)
      }).sum //replacing already known values and summing them
//      println()
//      println(row(i)+"x + "+v+" = "+auxiliar_y(i))
//      println(row(i)+"x = "+(auxiliar_y(i)-v))
      val xi = (auxiliar_y(i) - v)/row(i)
//      println("x = "+xi)
//      println()
      solution = solution :+ xi
    }

    solution = solution.reverse

    val reorderedX = for(i<-indexes) yield solution(i)
    new Vector(reorderedX:_*)
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

    setTotalPivote
    val x = solve(A,y)
    println("\n"+x)
  }
}
