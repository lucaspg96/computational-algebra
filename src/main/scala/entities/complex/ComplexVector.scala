package entities.complex

import scala.math.sqrt

class ComplexVector(vs: Complex*) {
  val values = vs.toArray
  val length = values.length

  lazy val norm: Double = sqrt(values.map(x => !x).sum)
  lazy val normalized: ComplexVector = this map {case (_, v) => v/norm}

  def apply(i: Int): Complex = values(i)
  def set(i: Int, v: Complex): Unit = values(i) = v

  def asList: List[Complex] = values.toList

  def transpose: ComplexMatrix = {
    val T = new ComplexMatrix(length,1)

    for(i <- 0 until length) T set ((i,0),this(i))

    T
  }

  def asMatrix: ComplexMatrix = {
    val A = new ComplexMatrix(1,length)
    A setRow (0,this)
    A
  }

  def sum: Complex = values.foldLeft(Complex(0,0))((acc,v) => acc + v)

  override def toString: String = values mkString("[",",","]")

  def map(f: (Int,Complex) => Complex): ComplexVector =
    new ComplexVector((for((i,v) <- (0 until length).zip(values)) yield f(i,v)):_*)

  def ==(b:ComplexVector): Boolean = {
    if(length != b.length) false

    for(i <- 0 until length) if(this(i)!=b(i)) false

    true
  }

  def copy: ComplexVector = this map {case (_,v) => v}

  def mkString(p: String): String = values mkString(p)
  def mkString(s: String,p: String, e: String): String = values mkString(s,p,e)

  def +(b: ComplexVector): ComplexVector =
    if(length == b.length) this map {case(i,v) => v+b(i)}
    else throw new Error("Vectors must have same lenght")

  def -(b: ComplexVector): ComplexVector =
    if(length == b.length) this map {case(i,v) => v-b(i)}
    else throw new Error("Vectors must have same lenght")

  def *(n: Complex): ComplexVector = this map {case(_,v) => v*n}
  def *(n: Double): ComplexVector = this map {case(_,v) => v * new Complex(n)}

  def /(n: Complex): ComplexVector = this map {case(_,v) => v/n}
  def /(n: Double): ComplexVector = this map {case(_,v) => v / new Complex(n)}

  //Scalar product
  def *(b: ComplexVector): Complex =
    if(length == b.length) (this map {case(i,v) => v*b(i)}).sum
    else throw new Error("Vectors must have same lenght")

  //Elements product
  def **(b: ComplexVector): ComplexVector =
    if(length == b.length) this map {case(i,v) => v*b(i)}
    else throw new Error("Vectors must have same lenght")

  //Cross product
  def ^(b: ComplexVector): ComplexVector =
    if(length!=3 || length!=b.length) throw new Error("Operation implemented only for 3d vectors")
    else new ComplexVector((this(1)*b(2)) - (this(2)*b(1)), this(2)*b(0) - this(0)*b(2), this(0)*b(1) - this(1)*b(0))

}
