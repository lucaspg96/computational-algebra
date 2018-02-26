object ProductTime extends App {
  val m = 10000
  val n = 10000

  val A = Array.ofDim[Double](m,n)
  val x = Array.ofDim[Double](n)

  val y = Array.ofDim[Double](m)

  val t0 = System.nanoTime()

  for(i <- 0 until m){
    for(k <- 0 until n) {
      y(i) += A(i)(k)*x(k)
    }
  }

  val t1 = System.nanoTime()
  println("Time to multiplicate a matrix of shape "+(m,n)+" by a vector of length "+n+" by the matrix row is: "+(t1-t0)/1000000+" ms")

  val t2 = System.nanoTime()

  for(k <- 0 until n){
    for(i <- 0 until m) {
      y(i) += A(i)(k)*x(k)
    }
  }

  val t3 = System.nanoTime()
  println("Time to multiplicate a matrix of shape "+(m,n)+" by a vector of length "+n+" by the matrix column is: "+(t3-t2)/1000000+" ms")
}
