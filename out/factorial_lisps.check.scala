import lisps._
import ast._
/*****************************************
Emitting Generated Code
*******************************************/
class Snippet extends ((Value)=>(Value)) {
  def apply(x20:Value): Value = {
    var x21: Value = I(0)
    val x22 = {x23: (Value) =>
      val x24 = apply_primitive_on_values("car", List(x23))
      var x26: Value = x24
      val x27 = apply_primitive_on_values("<", List(x24, I(2)))
      val x28 = x27 == B(false)
      val x34 = if (x28) {
        val x30 = x21
        val x29 = apply_primitive_on_values("-", List(x24, I(1)))
        val x31 = x30.asInstanceOf[Clo].fun(valueOf(List(x29)))
        val x32 = apply_primitive_on_values("*", List(x24, x31))
        x32
      } else {
        x24
      }
      x34: Value
    }
    val x36 = Clo(x22)
    x21 = x36
    val x38 = x36.asInstanceOf[Clo].fun(valueOf(List(x20)))
    x38
  }
}
/*****************************************
End of Generated Code
*******************************************/
