package linearSystemSolvers.qr

import entities.Matrix
import helpers.MatrixHelper

trait JacobiOrtogonalization extends QRConstructor {

  def calculateQR(A: Matrix): (Matrix,Matrix) = {
    val (m,n) = A.shape
    var Jt = MatrixHelper.getIdentity(m)
    var Ai = A copy

    do {
      for {
        j <- 0 until n
        i <- j + 1 until m
      } {
        val JijT = buildJijT(Ai, i, j)
        Ai = JijT * Ai
        Jt = JijT * Jt

        println("\nAi:")
        println(Ai)

//        println("\nJ:")
//        println(Jt)
      }
      println(noise(Ai))
          }while(noise(Ai) > 0.001)
//    }while(false)
    val R = Jt * A
//    println("JQ:\n"+Jt.transpose*R)
    (Jt.transpose,R)
  }

  def buildJijT(A: Matrix, i: Int, j: Int): Matrix = {
    val (m,n) = A.shape
    val J = MatrixHelper.getIdentity(m)

    val theta = if(A(j)(j)!=0) math.atan(A(i)(j)/A(j)(j)) else math.Pi/2

    J set ((j,j),math.cos(theta))
    J set ((j,i),math.sin(theta))
    J set ((i,j),-math.sin(theta))
    J set ((i,i),math.cos(theta))

    J
  }

  def noise(A: Matrix): Double =
    math.sqrt((for{
      j <- 0 until A.shape._1
      i <- j+1 until A.shape._2
    } yield A(i)(j) * A(i)(j)) sum)

}
