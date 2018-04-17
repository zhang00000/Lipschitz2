import breeze.linalg
import breeze.integrate
import math._

object LipschitzTest {
  type DF = (Double, Double) => Double

  private def sqr(v: Double) = v * v

  // given a batch of functions, return the function max(f1, f2, ..., fn)
  private def pointWiseMax(fl: DF*): DF = (v1, v2) => fl.map(f => f(v1, v2)).max

  // generate the u^\star function defined in the manuscript
  private def uStar(xk: Vector[(Double, Double)], uk: Vector[Double]): DF =
    pointWiseMax(xk.zip(uk).map { case (u1, u2) => new Lipschitz(u1, u2) }: _*)

  // check whether the points {xk} and corresponding {uk} satisfy the lipschitz constraints
  private def checkLipschitzness(xk: Vector[(Double, Double)], uk: Vector[Double]): Boolean = {
    for (i <- xk.indices; j <- i + 1 until uk.length)
      if (sqr(uk(i) - uk(j)) > sqr(xk(i)._1 - xk(j)._1) + sqr(xk(i)._2 - xk(j)._2))
        return false
    true
  }


  // given {xk} and {uk}, outputs a function that returns region number of a given point
  private def findRegionNumber(xk: Vector[(Double, Double)], uk: Vector[Double]): (Double, Double) => Int = {
    (v1, v2) => {
      xk.indices.foldLeft((Double.NegativeInfinity, -1)) { case ((v, i), j) =>
        val u = new Lipschitz(xk(j), uk(j))(v1, v2)
        if (u > v) (u, j) else (v, i)
      }._2
    }
  }

  // given a region finder, outputs a predicate
  private def isInRk(k: Int, regionFinder: (Double, Double) => Int): (Double, Double) => Boolean =
    regionFinder(_,_) == k

  def run(): Unit = {
    val xs = (-3.0 to 4.0 by 0.1).toVector
    val ys = (-3.0 to 3.0 by 0.1).toVector


    val xk = Vector((0.0, 1.0), (1.0, 0.0), (-0.5, -0.3))
    val uk = Vector(0.8, 0.2, 0.4)
    val f3 = uStar(xk, uk)

    val regionFinder = findRegionNumber(xk, uk)

    println(regionFinder(1.3, 0.6))
    //    MyPlotUtils.BasicPlot3D(xs, ys, f3)
  }
}
