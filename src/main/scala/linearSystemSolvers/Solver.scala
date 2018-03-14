package linearSystemSolvers

import entities.{Matrix, Vector}

trait Solver {
  def solve(A: Matrix, y: Vector): Vector
}
