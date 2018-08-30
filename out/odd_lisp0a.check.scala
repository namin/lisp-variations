/*****************************************
Emitting Generated Code
*******************************************/
class Snippet extends ((Int)=>(Int)) {
  def apply(x25:Int): Int = {
    var x12 = null.asInstanceOf[scala.Function1[Int, Int]]
    var x1 = null.asInstanceOf[scala.Function1[Int, Int]]
    x1 = {x2: (Int) =>
      var x3: Int = x2
      val x4 = x2 == 0
      val x5 = if (x4) {
        1
      } else {
        0
      }
      val x6 = x5 == 0
      val x10 = if (x6) {
        val x7 = x2 - 1
        val x8 = x12(x7)
        x8
      } else {
        1
      }
      x10: Int
    }
    x12 = {x13: (Int) =>
      var x14: Int = x13
      val x15 = x13 == 0
      val x16 = if (x15) {
        1
      } else {
        0
      }
      val x17 = x16 == 0
      val x21 = if (x17) {
        val x18 = x13 - 1
        val x19 = x1(x18)
        x19
      } else {
        0
      }
      x21: Int
    }
    val x26 = x12(x25)
    x26
  }
}
/*****************************************
End of Generated Code
*******************************************/
