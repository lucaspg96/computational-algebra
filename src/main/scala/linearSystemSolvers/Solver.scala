package linearSystemSolvers

import entities.real.{Matrix, Vector}

trait Solver {
  def solve(A: Matrix, y: Vector): Vector
}
