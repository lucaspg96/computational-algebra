package linearSystemSolvers.qr

import entities.real.Matrix

trait QRConstructor {
  var currentMatrix: Matrix = new Matrix(1)
  var Q,R: Matrix = _

  def getQR(A: Matrix): (Matrix,Matrix) = {
    if(!(A==currentMatrix)) {
      val qr = calculateQR(A)
      Q = qr._1
      R = qr._2
    }
    (Q,R)
  }

  def calculateQR(A: Matrix): (Matrix, Matrix)
}
