object Integrate_2D {

  def trapezoid(function: (Double, Double)=>Double,
                start:(Double, Double),
                end:(Double, Double),
                accuracy:(Int, Int)) : Double = {
    val step1 = (end._1 - start._1) / accuracy._1
    val step2 = (end._2 - start._2) / accuracy._2
    val r1 = 0 to accuracy._1
    val r2 = 0 to accuracy._2
    val cij: (Int, Int) => Double = (i, j) => {
      if ((i == 0 || i == accuracy._1) && (j == 0 || j == accuracy._2)) 0.25
      else if ((i == 0 || i == accuracy._1) || (j == 0 || j == accuracy._2)) 0.5
      else 1.0
    }
    val s = step1 * step2
    r1.par.map(i => r2.map(j=> cij(i, j)*function(start._1 + i * step1, start._2 + j * step2)).sum).sum*s
  }

  def trapezoidNormed(function: NormedVector=>Double,
                      start:(Double, Double),
                      end:(Double, Double),
                      accuracy:(Int, Int)) : Double = {
    val f: (Double, Double) => Double = (v1, v2) => function(new NormedVector(v1, v2))
    trapezoid(f, start, end, accuracy)
  }

}
