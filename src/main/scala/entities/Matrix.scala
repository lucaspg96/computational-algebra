package entities

class Matrix(m: Int, n: Int) {
  val matrix = Array.ofDim[Double](m,n)

  def this(n: Int) = this(n,n)

  def shape: (Int,Int) = (m,n)

  def apply(i: Int)(j: Int): Double = matrix(i)(j)
  def set(i: Int, j: Int, v: Double): Unit = matrix(i)(j) = v

  def rowAsVector(r: Int): Vector = new Vector(matrix(r):_*)
  def columnAsVector(c: Int): Vector = new Vector((for(r <- matrix) yield r(c)):_*)

  def map(f: MatrixValue => Double): Matrix = {
    val result = new Matrix(m,n)

    for{
      i <- 0 until m
      j <- 0 until n
    } result.set(i,j,f(new MatrixValue(i,j,this(i)(j))))

    result
  }

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
}
