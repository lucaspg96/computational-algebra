import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import entities.{Vector,Matrix}

@RunWith(classOf[JUnitRunner])
class MatrixTest extends FunSuite {

  test("matrix get and set") {
    val m = new Matrix(2)
    assert(m.shape == (2,2))
    assert(m(0)(0)==0)

    m set (0,0,1)
    assert(m(0)(0)==1)
  }

  test("component as vector"){
    val m = new Matrix(2)
    m set(0,0,1)
    m set(0,1,2)
    m set(1,0,3)
    m set(1,1,4)

    assert(m.rowAsVector(0) == new Vector(1,2))
    assert(m.rowAsVector(1) == new Vector(3,4))

    assert(m.columnAsVector(0) == new Vector(1,3))
    assert(m.columnAsVector(1) == new Vector(2,4))
  }

  test("adding matrices"){
    val m1 = new Matrix(2)
    m1 set(0,0,1)
    m1 set(0,1,2)
    m1 set(1,0,3)
    m1 set(1,1,4)

    val m2 = new Matrix(2)
    m2 set(0,0,0)
    m2 set(0,1,1)
    m2 set(1,0,2)
    m2 set(1,1,3)

    val m3 = new Matrix(2)
    m3 set(0,0,1)
    m3 set(0,1,3)
    m3 set(1,0,5)
    m3 set(1,1,7)

    assert( m1+m2 == m3)
  }

  test("subtracting matrices"){
    val m1 = new Matrix(2)
    m1 set(0,0,1)
    m1 set(0,1,2)
    m1 set(1,0,3)
    m1 set(1,1,4)

    val m2 = new Matrix(2)
    m2 set(0,0,0)
    m2 set(0,1,1)
    m2 set(1,0,2)
    m2 set(1,1,3)

    val m3 = new Matrix(2)
    m3 set(0,0,1)
    m3 set(0,1,1)
    m3 set(1,0,1)
    m3 set(1,1,1)

    assert( m1-m2 == m3)
  }

  test("multiplying matrices"){
    val m1 = new Matrix(2)
    m1 set(0,0,1)
    m1 set(0,1,2)
    m1 set(1,0,3)
    m1 set(1,1,4)

    val m2 = new Matrix(2)
    m2 set(0,0,0)
    m2 set(0,1,1)
    m2 set(1,0,2)
    m2 set(1,1,3)

    val m3 = new Matrix(2)
    m3 set(0,0,4)
    m3 set(0,1,7)
    m3 set(1,0,8)
    m3 set(1,1,5)

    assert( m1*m2 == m3)
  }

}
