import org.scalatest.{FunSuite, Matchers}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.math.abs
import entities.real.{Matrix, Vector}
import helpers.real.MatrixHelper

@RunWith(classOf[JUnitRunner])
class MatrixTest extends FunSuite with Matchers {

  test("loading matrix"){
    val m = MatrixHelper.loadFromFile(getClass.getResource("/test_matrix.txt").getPath)

    val m1 = new Matrix(2)
    m1 set((0,0),1)
    m1 set((0,1),2)
    m1 set((1,0),3)
    m1 set((1,1),4)

    assert(m==m1)
  }

  test("matrix get and set") {
    val m = new Matrix(2)
    assert(m.shape == (2,2))
    assert(m(0)(0)==0)

    m set ((0,0),1)
    assert(m(0)(0)==1)
  }

  test("set row") {
    val m1 = new Matrix(2)
    m1 set((0,0),1)
    m1 set((0,1),2)
    m1 set((1,0),3)
    m1 set((1,1),4)

    m1 setRow(1,new Vector(3,4))

    val m2 = new Matrix(2)
    m2 set((0,0),3)
    m2 set((0,1),4)
    m2 set((1,0),3)
    m2 set((1,1),4)

    assert(m1==m2)
  }

  test("transposing matrix"){
    val m1 = new Matrix(2)
    m1 set((0,0),1)
    m1 set((0,1),2)
    m1 set((1,0),3)
    m1 set((1,1),4)

    val m2 = new Matrix(2)
    m2 set((0,0),1)
    m2 set((0,1),3)
    m2 set((1,0),2)
    m2 set((1,1),4)

    assert(m1.transpose == m2)
  }

  test("matrix determinant"){
    val m = new Matrix(3)
    m set ((0,0),0)
    m set ((0,1),2)
    m set ((0,2),3)
    m set ((1,0),4)
    m set ((1,1),5)
    m set ((1,2),6)
    m set ((2,0),7)
    m set ((2,1),8)
    m set ((2,2),9)

    assert(abs(m.determinant-3) <= 0.0001)
  }

  test("component as vector"){
    val m = new Matrix(2)
    m set((0,0),1)
    m set((0,1),2)
    m set((1,0),3)
    m set((1,1),4)

    assert(m.rowAsVector(0) == new Vector(1,2))
    assert(m.rowAsVector(1) == new Vector(3,4))

    assert(m.columnAsVector(0) == new Vector(1,3))
    assert(m.columnAsVector(1) == new Vector(2,4))
  }

  test("adding matrices"){
    val m1 = new Matrix(2)
    m1 set((0,0),1)
    m1 set((0,1),2)
    m1 set((1,0),3)
    m1 set((1,1),4)

    val m2 = new Matrix(2)
    m2 set((0,0),0)
    m2 set((0,1),1)
    m2 set((1,0),2)
    m2 set((1,1),3)

    val m3 = new Matrix(2)
    m3 set((0,0),1)
    m3 set((0,1),3)
    m3 set((1,0),5)
    m3 set((1,1),7)

    assert( m1+m2 == m3)
  }

  test("subtracting matrices"){
    val m1 = new Matrix(2)
    m1 set((0,0),1)
    m1 set((0,1),2)
    m1 set((1,0),3)
    m1 set((1,1),4)

    val m2 = new Matrix(2)
    m2 set((0,0),0)
    m2 set((0,1),1)
    m2 set((1,0),2)
    m2 set((1,1),3)

    val m3 = new Matrix(2)
    m3 set((0,0),1)
    m3 set((0,1),1)
    m3 set((1,0),1)
    m3 set((1,1),1)

    assert( m1-m2 == m3)
  }

  test("multiplying matrices"){
    val m1 = new Matrix(2)
    m1 set((0,0),1)
    m1 set((0,1),2)
    m1 set((1,0),3)
    m1 set((1,1),4)

    val m2 = new Matrix(2)
    m2 set((0,0),0)
    m2 set((0,1),1)
    m2 set((1,0),2)
    m2 set((1,1),3)

    val m3 = new Matrix(2)
    m3 set((0,0),4)
    m3 set((0,1),7)
    m3 set((1,0),8)
    m3 set((1,1),5)

    val m4 = new Matrix(2)
    m4 set((0,0),3)
    m4 set((0,1),6)
    m4 set((1,0),9)
    m4 set((1,1),12)

    assert( m1*m2 == m3)
    assert (m1*3 == m4)
    assert(m1 * new Vector(1,2) == new Vector(5,11))
  }

  test("inverse matrix"){
    val A: Matrix = new Matrix(3)
    A set ((0,0),1)
    A set ((0,1),1)
    A set ((0,2),-1)

    A set ((1,0),1)
    A set ((1,1),-2)
    A set ((1,2),5)

    A set ((2,0),4)
    A set ((2,1),1)
    A set ((2,2),4)

    val I: Matrix = new Matrix(3)
    I set ((0,0),2.1666666666666665)
    I set ((0,1),0.8333333333333333)
    I set ((0,2),-0.5)

    I set ((1,0),-2.6666666666666665)
    I set ((1,1),-1.3333333333333333)
    I set ((1,2),1.0)

    I set ((2,0),-1.5)
    I set ((2,1),-0.5)
    I set ((2,2),0.5)

    assert(A.getInverse == I)
  }
}
