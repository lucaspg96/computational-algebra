package entities

import scala.math.{sqrt}

class Vector(vs: Double*) {
  val values = vs.toArray
  val length = values.length

  lazy val norm: Double = sqrt (values map (x => x*x) sum)
  lazy val normalized: Vector = this map {case (_, v) => v/norm}

  def apply(i: Int): Double = values(i)
  def set(i: Int, v: Double): Unit = values(i) = v

  def asList: List[Double] = values.toList

  def transpose: Matrix = {
    val T = new Matrix(length,1)

    for(i <- 0 until length) T set ((i,0),this(i))

    T
  }

  def asMatrix: Matrix = {
    val A = new Matrix(1,length)
    A setRow (0,this)
    A
  }

  def sum: Double = values.sum

  override def toString: String = values mkString("[",",","]")

  def map(f: (Int,Double) => Double): Vector =
    new Vector((for((i,v) <- (0 until length).zip(values)) yield f(i,v)):_*)

  def ==(b:Vector): Boolean = {
    if(length != b.length) false

    for(i <- 0 until length) if(this(i)!=b(i)) false

    true
  }

  def copy: Vector = this map {case (_,v) => v}

  def mkString(p: String): String = values mkString(p)
  def mkString(s: String,p: String, e: String): String = values mkString(s,p,e)

  def +(b: Vector): Vector =
    if(length == b.length) this map {case(i,v) => v+b(i)}
    else throw new Error("Vectors must have same lenght")

  def -(b: Vector): Vector =
    if(length == b.length) this map {case(i,v) => v-b(i)}
    else throw new Error("Vectors must have same lenght")

  def *(n: Double): Vector = this map {case(_,v) => v*n}

  //Scalar product
  def *(b: Vector): Double =
    if(length == b.length) (this map {case(i,v) => v*b(i)}).sum
    else throw new Error("Vectors must have same lenght")

  //Elements product
  def **(b: Vector): Vector =
    if(length == b.length) this map {case(i,v) => v*b(i)}
    else throw new Error("Vectors must have same lenght")

  //Cross product
  def ^(b: Vector): Vector =
    if(length!=3 || length!=b.length) throw new Error("Operation implemented only for 3d vectors")
    else new Vector(this(1)*b(2) - this(2)*b(1), this(2)*b(0) - this(0)*b(2), this(0)*b(1) - this(1)*b(0))

}