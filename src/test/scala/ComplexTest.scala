import entities.complex.Complex
import org.scalatest.{FunSuite, Matchers}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ComplexTest extends FunSuite {

  test("Equality"){
    val a = Complex(1,2)
    val b = Complex(1,2)

    assert(a==b)
  }

  test("Sum"){
    val a = Complex(1,2)
    val b = Complex(1,2)
    val c = a + b
    val d = a + 3

    assert(c==Complex(2,4))
    assert(d==Complex(4,2))
  }

  test("Subtraction"){
    val a = Complex(1,2)
    val b = Complex(1,2)
    val c = a - b
    val d = a - 3

    assert(c==new Complex(0))
    assert(d==Complex(-2,2))
  }

  test("Product"){
    val a = Complex(2,-1)
    val b = Complex(3,4)
    val c = a * b
    val d = a * 3

    assert(c==Complex(10,5))
    assert(d==Complex(6,-3))
  }

  test("Division"){
    val a = new Complex(3)
    val b = Complex(2,1)
    val c = a / b
    val d = b / 3

    assert(c==Complex(6.0/5.0,-3.0/5.0))
    assert(d==Complex(2.0/3.0,1.0/3.0))
  }

}
