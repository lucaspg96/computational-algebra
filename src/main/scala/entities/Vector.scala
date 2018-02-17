package entities

import scala.math.{sqrt}

class Vector(values: Double*) {
  val length = values.length

  lazy val norm: Double = sqrt (values map (x => x*x) sum)
  lazy val normalized: Vector = new Vector(this map {case (_, v) => v/norm}:_*)

  def this(n: Int, v: Double = 0) = this(Array.fill[Double](n)(v))

  def apply(i: Int): Double = values(i)
  def set(i: Int, v: Double): Unit = values(i) = v

  def sum: Double = values.sum

  def map[A](f: (Int,Double) => A) =
    for((i,v) <- (0 until length).zip(values)) yield f(i,v)

  def +(b: Vector): Vector =
    if(length == b.length) new Vector(this map {case(i,v) => v+b(i)}:_*)
    else throw new Error("Vectors must have same lenght")

  def -(b: Vector): Vector =
    if(length == b.length) new Vector(this map {case(i,v) => v-b(i)}:_*)
    else throw new Error("Vectors must have same lenght")

  //Scalar product
  def *(b: Vector): Double =
    if(length == b.length) this map {case(i,v) => v*b(i)}.sum
    else throw new Error("Vectors must have same lenght")

  //Elements product
  def **(b: Vector): Vector =
    if(length == b.length) new Vector(this map {case(i,v) => v*b(i)}:_*)
    else throw new Error("Vectors must have same lenght")

  //Cross product
  def ^(b: Vector): Vector =
    if(length!=3 || length!=b.length) throw new Error("Operation implemented only for 3d vectors")
    else new Vector(this(1)*b(2) - this(2)*b(1), this(2)*b(0) - this(0)*b(2), this(0)*b(1) - this(1)*b(0))

}
