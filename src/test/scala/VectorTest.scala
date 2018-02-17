import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import entities.{VectorHelper,Vector}

@RunWith(classOf[JUnitRunner])
class VectorTest extends FunSuite{

  test("vector basic methods") {
    val v = new Vector(1,2,3)
    assert(v.length==3)
    assert(v(0)==1)
    assert(v.sum == 6)

    v.set(1,2)
    assert(v(1) == 2)

    val v2 = VectorHelper.createVector(5,1)
    assert(v2.length == 5)
    assert(v2(3) == 1)
    assert(v2.sum == 5)

    assert(new Vector(1,2,3) == new Vector(1,2,3))
  }

  test("normalized vector"){
    val v = new Vector(1,2,3)

    assert(v.norm == 3.7416573867739413)
    assert(v.normalized == new Vector(0.267261, 0.534522, 0.801784))
  }

  test("mapping vector"){
    val v = new Vector(1,2,3)
    val vm = v map {case(i,_) => i}

    assert(vm == new Vector(0,1,2))
  }

  test("adding vectors"){
    val v1 = new Vector(1,2,3)
    val v2 = new Vector(4,5,6)

    assert((v1+v2) == new Vector(5,7,9))
  }

  test("subtracting vectors"){
    val v1 = new Vector(1,2,3)
    val v2 = new Vector(4,5,6)

    assert((v2-v1) == new Vector(3,3,3))
  }

  test("vector products"){
    val v1 = new Vector(1,2,3)
    val v2 = new Vector(1,1,1)

    assert(v1*v2 == 6)
    assert(v1**v2 == new Vector(1,2,3))
    assert((v1^v2) == new Vector(-1,2,-1))
  }


}
