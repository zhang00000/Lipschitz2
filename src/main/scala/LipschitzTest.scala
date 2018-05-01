import breeze.linalg
import breeze.integrate
import math._

object LipschitzTest {

//  private def sqr(v: Double) = v * v

  // given a batch of functions, return the function max(f1, f2, ..., fn)
  private def pointWiseMax [A <: Any](fl: (A => Double)*): A => Double =
    v => fl.map(f => f(v)).max

  // generate the u^\star function defined in the manuscript
  private def uStar(xk: Vector[NormedVector], uk: Vector[Double]): NormedVector => Double =
    pointWiseMax(xk.zip(uk).map { case (u1, u2) => new Lipschitz(u1, u2) }: _*)

  // check whether the points {xk} and corresponding {uk} satisfy the Lipschitz constraints
  private def checkLipschitzness(xk: Vector[NormedVector], uk: Vector[Double]): Boolean = {
    for (i <- xk.indices; j <- i + 1 until uk.length)
      if (abs(uk(i) - uk(j)) > (xk(i) - xk(j)).norm)
        return false
    true
  }


  // given {xk} and {uk}, outputs a function that returns region number of a given point
  private def generateRegionFinder(xk: Vector[NormedVector], uk: Vector[Double]): NormedVector => Int = {
    v1 => {
      xk.indices.foldLeft((Double.NegativeInfinity, -1)) { case ((v, i), j) =>
        val u = new Lipschitz(xk(j), uk(j))(v1)
        if (u > v) (u, j) else (v, i)
      }._2
    }
  }

  // given a region finder, outputs a predicate
  private def isInRk(k: Int, regionFinder: NormedVector => Int): NormedVector => Boolean =
    regionFinder(_) == k

  def run(): Unit = {
    val xs = (-3.0 to 4.0 by 0.1).toVector
    val ys = (-3.0 to 3.0 by 0.1).toVector


//    val xk = Vector(new NormedVector(0.0, 1.0),
//      new NormedVector(1.0, 0.0),
//      new NormedVector(-0.5, -0.3))
//    val uk = Vector(0.8, 0.2, 0.4)
//    val f3 = uStar(xk, uk)
    val xk = Vector(new NormedVector(0.0, 1.0))
    val uk = Vector(1.0)
    val f3 = uStar(xk, uk)

    val regionFinder = generateRegionFinder(xk, uk)

    println(regionFinder(new NormedVector(1.3, 0.6)))
        MyPlotUtils.BasicPlot3D(xs, ys, (v1, v2) => f3(new NormedVector(v1, v2)))
  }
}
