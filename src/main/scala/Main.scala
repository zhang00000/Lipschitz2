import breeze.integrate._
import scala.math._

object Main {
  def main(args: Array[String]): Unit = {
//    LipschitzTest.run()
    test1()
  }


  def test1(): Unit = {
    var gd = new SubGradient(new NormedVector(4.0))
    println(gd.x)
    gd.update(0.01)
    println(gd.x)
    println(gd.k)



  }


}
