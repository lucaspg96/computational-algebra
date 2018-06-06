package linearSystemSolvers.qr

import entities.real.{Matrix, Vector}
import helpers.real.{MatrixHelper, VectorHelper}

trait HouseHolderOrtogonalization extends QRConstructor {
  def calculateQR(A: Matrix): (Matrix,Matrix) = {
    val (m,n) = A.shape
    var H = MatrixHelper.getIdentity(m)
    var Aj = A copy

    println("\nH:")
    println(H)

    for(j <- 0 until n){
      val Hj = buildHj(Aj,j)
//      println("\nH"+j+":")
//      println(Hj)
//      println("Is ortogonal? "+Hj.isOrtogonal)
      Aj = Hj * Aj
      H = H * Hj
//      println("\nUpdating H:")
//      println(H)
//      println("Is ortogonal? "+H.isOrtogonal)
    }

    val R = H.transpose * A

    (H,R)
  }

  private def buildHj(A: Matrix, j: Int): Matrix = {
//    println("\nCreating H"+j)
    val (m,_) = A.shape
//    println("\nA:")
//    println(A)

    val V = VectorHelper.createVector(m)
    val V2 = VectorHelper.createVector(m)

    for(i <- j until m) V set (i,A(i)(j))
//    println("\nV:")
//    println(V)

    val vj = if(V(j)>0) -V.norm else V.norm
    V2 set (j,vj)
//    println("\nV2:")
//    println(V2)

    val n: Vector = (V - V2).normalized
//    println("\nn:")
//    println(n)
//    println("\nn*nt:")
//    println(n.transpose*n.asMatrix)
//    println("\n2*(n*nt):")
//    println((n.transpose*n.asMatrix)*2)
    MatrixHelper.getIdentity(m) - ((n.transpose*n.asMatrix)*2)
  }
}
