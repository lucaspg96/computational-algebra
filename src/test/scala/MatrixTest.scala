import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import entities._

@RunWith(classOf[JUnitRunner])
class MatrixTest extends FunSuite {

  test("matrix get and set") {
    val m = new Matrix(2)
    assert(m.shape == (2,2))
    assert(m(0)(0)==0)

    m set (0,0,1)
    assert(m(0)(0)==1)

  }



}
