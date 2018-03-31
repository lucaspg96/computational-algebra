package linearSystemSolvers.qr

import entities.{Matrix, Vector}
import helpers.VectorHelper

trait GramSchmidtOrtogonalization extends QRConstructor {

  def calculateQR(A: Matrix): (Matrix,Matrix) = {
    val(m,n) = A.shape

    var Qs = List[Vector]()

    for{
      j <- 0 until n
    }{
      if(j==0) Qs :+= A.columnAsVector(0).normalized
      else{
        val sum: Vector = Qs.foldLeft(VectorHelper.createVector(m)){ (s, v) => {
//          println(s"$s, $v")
          s+v*(A.columnAsVector(j)*v)
        }}
        val Vi = A.columnAsVector(j) - sum
        Qs :+= Vi.normalized
      }
    }
  println(Qs mkString "\n")
    val Q = new Matrix(m,n)
    for(j <- 0 until n) Q setColumn(j,Qs(j))

    val R = Q.transpose * A

    (Q,R)
  }
}
