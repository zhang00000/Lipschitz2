class Lipschitz(private val p: (Double, Double),
                     private val zp: Double,
                     private val L: Double = -1.0)

  extends ((Double, Double) => Double) {

  override def apply(v1: Double, v2: Double): Double
    = zp + L * math.sqrt((v1 - p._1) * (v1 - p._1) + (v2 - p._2) * (v2 - p._2))

}
