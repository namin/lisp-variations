/*****************************************
Emitting Generated Code
*******************************************/
class Snippet extends ((Int)=>(Int)) {
  def apply(x15:Int): Int = {
    var x1 = null.asInstanceOf[scala.Function1[Int, Int]]
    x1 = {x2: (Int) =>
      var x3: Int = x2
      val x4 = x2 < 2
      val x5 = if (x4) {
        1
      } else {
        0
      }
      val x6 = x5 == 0
      val x11 = if (x6) {
        val x7 = x2 - 1
        val x8 = x1(x7)
        val x9 = x2 * x8
        x9
      } else {
        x2
      }
      x11: Int
    }
    val x16 = x1(x15)
    x16
  }
}
/*****************************************
End of Generated Code
*******************************************/
