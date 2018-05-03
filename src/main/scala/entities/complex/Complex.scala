package entities.complex

import scala.math.{abs, pow, sqrt}

case class Complex(a: Double, b: Double) extends Ordered[Complex]{

  private val modulus = sqrt(pow(a, 2) + pow(b, 2))

  def unary_! : Double = modulus
  def unary_- : Complex = Complex(-a,-b)

  def compare(that: Complex) = !this compare !that

  def this(a: Double) = this(a,0)

  def conjugate: Complex = Complex(a,-b)

  def real: Double = a
  def i: Double = b

  def == (c: Complex): Boolean = a == c.real && b == c.i
  def == (n: Double): Boolean = this == new Complex(n)

  def + (c: Complex): Complex = Complex(a+c.real,b+c.i)
  def + (n: Double): Complex = this + new Complex(n)

  def - (c: Complex): Complex = Complex(a-c.real,b-c.i)
  def - (n: Double): Complex = this - new Complex(n)

  def * (c: Complex): Complex = Complex(a*c.real - b*c.i, b*c.a + a*c.i)
  def * (n: Double): Complex = this * new Complex(n)

  def / (c: Complex): Complex = {
    require(c.real != 0 || c.i != 0)
    val d = pow(c.real,2) + pow(c.i,2)
    (this*c.conjugate)/ d
  }
  def / (n: Double): Complex = Complex(a/n, b/n)

  override def toString: String =
    this match {
      case Complex(0,0) => "0"
      case Complex(x,0) => x.toString
      case Complex(0,x) => s"${x}i"
      case Complex(x,y) =>
        if(y>0) s"$x + ${y}i"
        else s"$x - ${abs(y)}i"
    }

}
