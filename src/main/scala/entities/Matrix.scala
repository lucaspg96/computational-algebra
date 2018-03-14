package entities

import helpers.VectorHelper

class Matrix(m: Int, n: Int) {
  val matrix = for(_ <- 1 to m) yield VectorHelper.createVector(n)

  def this(n: Int) = this(n,n)

  def shape: (Int,Int) = (m,n)

  def apply(i: Int)(j: Int): Double = matrix(i)(j)
  def set(p: (Int,Int), v: Double): Unit = matrix(p._1).set(p._2,v)

  def rowAsVector(r: Int): Vector = matrix(r)
  def columnAsVector(c: Int): Vector = new Vector((for(r <- matrix) yield r(c)):_*)

  override def toString: String = (for(row <- matrix) yield row mkString("|",",","|")) mkString("\n")

  def copy: Matrix = this map {case MatrixValue(_,_,v) => v}

  def map(f: MatrixValue => Double): Matrix = {
    val result = new Matrix(m,n)

    for{
      i <- 0 until m
      j <- 0 until n
    } result.set((i,j),f(new MatrixValue(i,j,this(i)(j))))

    result
  }

  def transpose: Matrix = this map {case MatrixValue(i,j,_) => matrix(j)(i)}

  def +(b: Matrix): Matrix =
    if (b.shape == shape) this map {case MatrixValue(i,j,v) => v + b(i)(j)}
    else throw new Error("Matrices must have same dimensions")

  def -(b: Matrix): Matrix =
    if (b.shape == shape) this map {case MatrixValue(i,j,v) => v - b(i)(j)}
    else throw new Error("Matrices must have same dimensions")

  def *(b: Matrix): Matrix =
    if(shape._2 == b.shape._1) this map {
      case MatrixValue(i,j,_) => this.rowAsVector(i)*b.columnAsVector(j)
    }
    else throw new Error("Can't multiply matrices with shapes "+shape+" and "+b.shape)

  def *(v: Vector): Vector = new Vector((for(i <- 0 until m) yield rowAsVector(i)*v):_*)

  def *(n: Double): Matrix = this map {case MatrixValue(_,_,v) => v*n}

  def ==(b: Matrix): Boolean = {
    for{
      i <- 0 until m
      j <- 0 until n
    } if(this(i)(j) != b(i)(j)) false

    true
  }

  private def partialPivote(k: Int): Matrix = {
    var swap = k

    for(i <- k+1 until n) {
      if (this (i)(i) > this (k)(k)) {
        swap = i
      }
    }

    this map {case MatrixValue(i,j,v) =>
      if(i==k) this(swap)(j) else if(i==swap) this(k)(j) else v
    }

  }

  def setRow(r: Int,row: Vector): Unit = {
    if(row.length!=n) throw new Error("Can't set row with length "+row.length+" on matrix with shape "+shape)

    for(c <- 0 until n) this.set((r,c),row(c))
  }

  def gaussianElimination: Matrix = {
    if(m!=n) throw new Error("Operation not defined to non-square matrices")

    var auxiliar_matrix = this copy
    var d = 1

    for{
      i <- 0 until n //row
      j <- i+1 until n //following row
    }{
      if(auxiliar_matrix(i)(i)==0) {
       throw new Error("Can't apply guassian elimination. 0 found during it")
      }

      val m = -auxiliar_matrix(j)(i)/auxiliar_matrix(i)(i)

      for(k <- i until n) {
        val v = auxiliar_matrix(j)(k) + m*auxiliar_matrix(i)(k)
        auxiliar_matrix.set((j,k),v)
      }
    }

    auxiliar_matrix
  }

  def gaussJordanElimination: Matrix = {
    var auxiliar_matrix = this.gaussianElimination

    for{
      i <- n-1 to 0 by -1 //row
      j <- i-1 to 0 by -1 //following row
    }{
      var baseRow = auxiliar_matrix.rowAsVector(i)
      if(baseRow(i)!=0){
        baseRow = baseRow*(1/baseRow(i))
        auxiliar_matrix.setRow(i,baseRow)
      }


      val targetRow = auxiliar_matrix.rowAsVector(j)

      val m =  -targetRow(i)

      val newRow = (baseRow*m)+targetRow
      auxiliar_matrix.setRow(j,newRow)

    }

    auxiliar_matrix
  }

  def determinant: Double = {
    if(m!=n) throw new Error("Operation not defined to non-square matrices")

    var auxiliar_matrix = this copy
    var d = 1

    for{
      i <- 0 until n //row
      j <- i+1 until n //following row
    }{
      if(auxiliar_matrix(i)(i)==0) {
        d = -d
        auxiliar_matrix = auxiliar_matrix.partialPivote(i)
      }

      val m = -auxiliar_matrix(j)(i)/auxiliar_matrix(i)(i)
      auxiliar_matrix.set((j,i),0)

      for(k <- i+1 until n) {
        val v = auxiliar_matrix(j)(k) + m*auxiliar_matrix(i)(k)
        auxiliar_matrix.set((j,k),v)
      }
    }

    (for(i <- 0 until n) yield auxiliar_matrix(i)(i)).product * d
  }

}
