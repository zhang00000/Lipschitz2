import scala.runtime.ScalaRunTime

trait Normed {
  def norm: Double
}

class NormedVector extends Normed {
  var arr : Array[Double] = Array[Double]()

  override def norm: Double = Math.sqrt(arr.map(x => x*x).sum)

  def this(array: Array[Double]) {
    this()
    arr = array
  }

  def this(d: Int) {
    this(new Array[Double](d))
  }

  def this(doubles: Double*) {
    this(doubles.length)
    for (i <- doubles.indices) arr(i) = doubles(i)
  }

  def +(that: NormedVector): NormedVector = {
    val r = new Array[Double](this.arr.length)
    for (i <- this.arr.indices) r(i) = this.arr(i) + that.arr(i)
    new NormedVector(r)
  }

  def -(that: NormedVector): NormedVector = {
    val r = new Array[Double](this.arr.length)
    for (i <- this.arr.indices) r(i) = this.arr(i) - that.arr(i)
    new NormedVector(r)
  }

  def *(that: Double): NormedVector = {
    val r = new Array[Double](this.arr.length)
    for (i <- this.arr.indices) r(i) = this.arr(i) * that
    new NormedVector(r)
  }

  override def toString: String = "(" + arr.mkString(", ") + ")"
}
