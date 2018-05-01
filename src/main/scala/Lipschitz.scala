
class Lipschitz(private val p : NormedVector,
                     private val zp: Double,
                     private val L: Double = -1.0)

  extends (NormedVector => Double) {

  override def apply(v: NormedVector): Double = zp + L * (p-v).norm

  def apply (doubles: Double*): Double = {
    val arr = new Array[Double](doubles.length)
    for (i<- arr.indices) arr(i) = doubles(i)
    apply(new NormedVector(arr))
  }
}
