package entities.complex

import helpers.complex.{ComplexMatrixHelper, ComplexVectorHelper}

class ComplexMatrix(m: Int, n: Int) {
  val matrix: Array[ComplexVector] = (for (_ <- 1 to m) yield ComplexVectorHelper.createVector(n)).toArray
  private lazy val inverse: Option[ComplexMatrix] = gaussJordanInverse

  def this(n: Int) = this(n, n)

  def shape: (Int, Int) = (m, n)

  def isSquare: Boolean = m==n

  def hasInverse: Boolean = inverse.isDefined

  def apply(i: Int)(j: Int): Complex = matrix(i)(j)

  def set(p: (Int, Int), v: Complex): Unit = matrix(p._1).set(p._2, v)
  def set(p: (Int, Int), v: Double): Unit = matrix(p._1).set(p._2, new Complex(v))

  def isSymmetric: Boolean = {
    for {
      i <- 0 until m
      j <- 0 until n
      if this (i)(j) != this (j)(i)
    } return false

    true
  }

  def rowAsVector(r: Int): ComplexVector = matrix(r)

  def columnAsVector(c: Int): ComplexVector = new ComplexVector((for (r <- matrix) yield r(c)): _*)

  def getInverse: ComplexMatrix = inverse.get

  override def toString: String = (for (row <- matrix) yield row mkString("|", ",", "|")) mkString "\n"

  def copy: ComplexMatrix = this map { case ComplexMatrixValue(_, _, v) => v }

  def map(f: ComplexMatrixValue => Complex): ComplexMatrix = {
    val result = new ComplexMatrix(m, n)

    for {
      i <- 0 until m
      j <- 0 until n
    } result.set((i, j), f(ComplexMatrixValue(i, j, this (i)(j))))

    result
  }

  def transpose: ComplexMatrix = this map { case ComplexMatrixValue(i, j, _) => matrix(j)(i) }

  def +(b: ComplexMatrix): ComplexMatrix =
    if (b.shape == shape) this map { case ComplexMatrixValue(i, j, v) => v + b(i)(j) }
    else throw new Error("Matrices must have same dimensions")

  def -(b: ComplexMatrix): ComplexMatrix =
    if (b.shape == shape) this map { case ComplexMatrixValue(i, j, v) => v - b(i)(j) }
    else throw new Error("Matrices must have same dimensions")

  def *(b: ComplexMatrix): ComplexMatrix =
    if (shape._2 == b.shape._1) {
      val A = new ComplexMatrix(m, b.shape._2)
      A map {
        case ComplexMatrixValue(i, j, _) => this.rowAsVector(i) * b.columnAsVector(j)
      }
    }
    else throw new Error("Can't multiply matrices with shapes " + shape + " and " + b.shape)

  def *(v: ComplexVector): ComplexVector = new ComplexVector((for (i <- 0 until m) yield rowAsVector(i) * v): _*)

  def *(n: Double): ComplexMatrix = this map { case ComplexMatrixValue(_, _, v) => v * n }

  def ==(b: ComplexMatrix): Boolean = {
    if (b.shape != shape) false

    else {
      for {
        i <- 0 until m
        j <- 0 until n
      } if (this (i)(j) != b(i)(j)) return false

      true
    }

  }

  def diagonalAsVector: ComplexVector = new ComplexVector((for (i <- 0 until m) yield this(i)(i)): _*)

  private def partialPivote(k: Int): ComplexMatrix = {
    var swap = k

    for (i <- k + 1 until n) {
      if (this (i)(i) > this (k)(k)) {
        swap = i
      }
    }

    this map { case ComplexMatrixValue(i, j, v) =>
      if (i == k) this (swap)(j) else if (i == swap) this (k)(j) else v
    }

  }

  def setRow(r: Int, row: ComplexVector): Unit =
    if (row.length != n) throw new Error("Can't set row with length " + row.length + " on matrix with shape " + shape)
    else for (c <- 0 until n) this.set((r, c), row(c))

  def setColumn(c: Int, column: ComplexVector): Unit =
    if (column.length != m) throw new Error("Can't set column with length " + column.length + " on matrix with shape " + shape)
    else for (r <- 0 until m) this.set((r, c), column(r))

  def determinant: Complex = {
    if (m != n) throw new Error("Operation not defined to non-square matrices")

    var auxiliar_matrix = this.copy
    var d = Complex(1,0)

    for {
      i <- 0 until n //row
      j <- i + 1 until n //following row
    } {
      if (auxiliar_matrix(i)(i) == 0) {
        d = -d
        auxiliar_matrix = auxiliar_matrix.partialPivote(i)
      }

      val m = -auxiliar_matrix(j)(i) / auxiliar_matrix(i)(i)
      auxiliar_matrix.set((j, i), 0)

      for (k <- i + 1 until n) {
        val v = auxiliar_matrix(j)(k) + m * auxiliar_matrix(i)(k)
        auxiliar_matrix.set((j, k), v)
      }
    }

    (for (i <- 0 until n) yield auxiliar_matrix(i)(i)).foldLeft(Complex(1,0))((acc,v) => acc*v) * d
  }

  def gaussJordanInverse: Option[ComplexMatrix] = {
    if (m != n) throw new Error("Only square matrices can have inverse!")

    val inverse = ComplexMatrixHelper.getIdentity(m)
    val auxiliar_matrix = copy

    //    println(auxiliar_matrix)
    //    println()
    //    println(inverse)
    //    println("---------------")
    for {
      i <- 0 until n //row
      j <- i + 1 until n //following row
    } {
      if (auxiliar_matrix(i)(i) == 0) {
        println("0 found at diagonal " + i)
        return None
      }

      val baseRow = auxiliar_matrix.rowAsVector(i) //row i
      val inverseBaseRow = inverse.rowAsVector(i) //inverse row i

      val targetRow = auxiliar_matrix.rowAsVector(j) //row j
      val inverseTargetRow = inverse.rowAsVector(j) //inverse row j

      val alpha: Complex = -auxiliar_matrix(j)(i) / auxiliar_matrix(i)(i) //signal is inverted

      val newRow = (baseRow * alpha) + targetRow
      auxiliar_matrix.setRow(j, newRow)

      val newInverseRow = (inverseBaseRow * alpha) + inverseTargetRow
      inverse.setRow(j, newInverseRow)

      //      println(auxiliar_matrix)
      //      println()
      //      println(inverse)
      //      println("---------------")
    }

    for {
      i <- n - 1 until 0 by -1 //row
      j <- i - 1 to 0 by -1 //upper row
    } {
      var baseRow = auxiliar_matrix.rowAsVector(i) //row i
      var inverseBaseRow = inverse.rowAsVector(i) //inverse row i

      //dividing row by pivot value
      inverseBaseRow = inverseBaseRow / baseRow(i)
      inverse.setRow(i, inverseBaseRow)

      baseRow = baseRow / baseRow(i)
      auxiliar_matrix.setRow(i, baseRow)

      val targetRow = auxiliar_matrix.rowAsVector(j) //row j
      val inverseTargetRow = inverse.rowAsVector(j) //inverse row j

      val alpha = -auxiliar_matrix(j)(i) //signal is inverted

      val newRow = (baseRow * alpha) + targetRow
      auxiliar_matrix.setRow(j, newRow)

      val newInverseRow = (inverseBaseRow * alpha) + inverseTargetRow
      inverse.setRow(j, newInverseRow)

      //      println(auxiliar_matrix)
      //      println()
      //      println(inverse)
      //      println("---------------")
    }

    Some(inverse)
  }

  def normalized: ComplexMatrix = {
    val A = new ComplexMatrix(m,n)
    for(c <- 0 until n) A setColumn(c, A columnAsVector c normalized)
    A
  }
}
