class SubGradient {
  var x: NormedVector = new NormedVector()
  var k = 0
  var ak: Int => Double = k => 1.0/(k + 1)
  var gradient: NormedVector => NormedVector = x => x * 0.5

  def this(initial: NormedVector) {
    this()
    x = initial
  }

  def update(): Double = {
    val v = gradient(x) * ak(k)
    x = x - v
    k += 1
    v.norm/ak(k)
  }

  def update(step: Int): Double = {
    (1 to step).map(_ => update()).last
  }

  def update(epsilon: Double): Double = {
    val ret = update()
    if (ret < epsilon) ret
    else update(epsilon)
  }

}
