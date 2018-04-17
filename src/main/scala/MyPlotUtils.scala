import co.theasi.plotly._

object MyPlotUtils {
  def BasicPlot3D(xs: Iterable[Double], ys: Iterable[Double],
                  function2: (Double, Double) => (Double) ): Unit = {
    val zs = ys.map { y => xs.map { x => function2(x, y) }}
    val p = ThreeDPlot().withSurface(xs, ys, zs)
    draw(p, "my-first-plot")
    println("End of Game")
  }
}
