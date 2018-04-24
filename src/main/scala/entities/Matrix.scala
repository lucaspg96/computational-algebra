package entities

import helpers.{MatrixHelper, VectorHelper}

class Matrix(m: Int, n: Int) {
  val matrix: Array[Vector] = (for (_ <- 1 to m) yield VectorHelper.createVector(n)).toArray
  private lazy val inverse: Option[Matrix] = gaussJordanInverse

  def this(n: Int) = this(n, n)

  def shape: (Int, Int) = (m, n)

  def isSquare: Boolean = m==n

  def hasInverse: Boolean = inverse.isDefined

  def apply(i: Int)(j: Int): Double = matrix(i)(j)

  def set(p: (Int, Int), v: Double): Unit = matrix(p._1).set(p._2, v)

  def isSymmetric: Boolean = {
    for {
      i <- 0 until m
      j <- 0 until n
      if this (i)(j) != this (j)(i)
    } return false

    true
  }

  def rowAsVector(r: Int): Vector = matrix(r)

  def columnAsVector(c: Int): Vector = new Vector((for (r <- matrix) yield r(c)): _*)

  def getInverse: Matrix = inverse.get

  override def toString: String = (for (row <- matrix) yield row mkString("|", ",", "|")) mkString "\n"

  def copy: Matrix = this map { case MatrixValue(_, _, v) => v }

  def map(f: MatrixValue => Double): Matrix = {
    val result = new Matrix(m, n)

    for {
      i <- 0 until m
      j <- 0 until n
    } result.set((i, j), f(MatrixValue(i, j, this (i)(j))))

    result
  }

  def isOrtogonal: Boolean = {
    for(c <- 0 until n) {
      if(math.abs(columnAsVector(c).norm-1)>0.0001) return false
    }

    return this * this.transpose == MatrixHelper.getIdentity(m)
  }

  def transpose: Matrix = this map { case MatrixValue(i, j, _) => matrix(j)(i) }

  def +(b: Matrix): Matrix =
    if (b.shape == shape) this map { case MatrixValue(i, j, v) => v + b(i)(j) }
    else throw new Error("Matrices must have same dimensions")

  def -(b: Matrix): Matrix =
    if (b.shape == shape) this map { case MatrixValue(i, j, v) => v - b(i)(j) }
    else throw new Error("Matrices must have same dimensions")

  def *(b: Matrix): Matrix =
    if (shape._2 == b.shape._1) {
      val A = new Matrix(m, b.shape._2)
      A map {
        case MatrixValue(i, j, _) => this.rowAsVector(i) * b.columnAsVector(j)
      }
    }
    else throw new Error("Can't multiply matrices with shapes " + shape + " and " + b.shape)

  def *(v: Vector): Vector = new Vector((for (i <- 0 until m) yield rowAsVector(i) * v): _*)

  def *(n: Double): Matrix = this map { case MatrixValue(_, _, v) => v * n }

  def ==(b: Matrix): Boolean = {
    if (b.shape != shape) false

    else {
      for {
        i <- 0 until m
        j <- 0 until n
      } if (this (i)(j) != b(i)(j)) return false

      return true
    }

  }

  def diagonalAsVector: Vector = new Vector((for (i <- 0 until m) yield this(i)(i)): _*)

  private def partialPivote(k: Int): Matrix = {
    var swap = k

    for (i <- k + 1 until n) {
      if (this (i)(i) > this (k)(k)) {
        swap = i
      }
    }

    this map { case MatrixValue(i, j, v) =>
      if (i == k) this (swap)(j) else if (i == swap) this (k)(j) else v
    }

  }

  def setRow(r: Int, row: Vector): Unit =
    if (row.length != n) throw new Error("Can't set row with length " + row.length + " on matrix with shape " + shape)
    else for (c <- 0 until n) this.set((r, c), row(c))

  def setColumn(c: Int, column: Vector): Unit =
    if (column.length != m) throw new Error("Can't set column with length " + column.length + " on matrix with shape " + shape)
    else for (r <- 0 until m) this.set((r, c), column(r))

  def determinant: Double = {
    if (m != n) throw new Error("Operation not defined to non-square matrices")

    var auxiliar_matrix = this.copy
    var d = 1

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

    (for (i <- 0 until n) yield auxiliar_matrix(i)(i)).product * d
  }

  def gaussJordanInverse: Option[Matrix] = {
    if (m != n) throw new Error("Only square matrices can have inverse!")

    val inverse = MatrixHelper.getIdentity(m)
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

      val alpha = -auxiliar_matrix(j)(i) / auxiliar_matrix(i)(i) //signal is inverted

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
      inverseBaseRow = inverseBaseRow * (1 / baseRow(i))
      inverse.setRow(i, inverseBaseRow)

      baseRow = baseRow * (1 / baseRow(i))
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
}
