package principalComponentAnalysis

import entities.real.Matrix
import singularValueDecomposition.SVD

object PCA {
  def apply(A: Matrix, sigma: Double): Matrix = {
    val X = A.centralized
    val covMatrix = X.transpose*X

    val svd = new SVD(covMatrix)

    val U = svd.U.toReal

    val S = svd.S.toReal

    val totalInfluence = S.diagonalAsVector.sum

    var partialInfluence = 0.0

    val components = S.diagonalAsVector.asList.takeWhile(l => {
      if((partialInfluence + l/totalInfluence) < sigma){
        partialInfluence += l/totalInfluence
        true
      } else false
    }).length

    val W = new Matrix(U.shape._1, components)

    for(j <- 0 until components) W.setColumn(j, U.columnAsVector(j))

    X*W
  }
}
