import lisps._
import ast._
/*****************************************
Emitting Generated Code
*******************************************/
class Snippet extends ((Value)=>(Value)) {
  def apply(x43:Value): Value = {
    var x44: Value = I(0)
    val x45 = {x46: (Value) =>
      val x47 = apply_primitive_on_values("car", List(x46))
      var x49: Value = x47
      x47: Value
    }
    val x51 = Clo(x45)
    x44 = x51
    var x53: Value = I(0)
    val x54 = {x55: (Value) =>
      val x56 = apply_primitive_on_values("car", List(x55))
      var x58: Value = x56
      val x59 = apply_primitive_on_values("eq?", List(x56, I(0)))
      val x60 = x59 == B(false)
      val x65 = if (x60) {
        val x62 = x44
        val x61 = apply_primitive_on_values("-", List(x56, I(1)))
        val x63 = x62.asInstanceOf[Clo].fun(valueOf(List(x61)))
        x63
      } else {
        B(true)
      }
      x65: Value
    }
    val x67 = Clo(x54)
    x53 = x67
    val x69 = {x70: (Value) =>
      val x71 = apply_primitive_on_values("car", List(x70))
      var x73: Value = x71
      val x74 = apply_primitive_on_values("eq?", List(x71, I(0)))
      val x75 = x74 == B(false)
      val x80 = if (x75) {
        val x77 = x53
        val x76 = apply_primitive_on_values("-", List(x71, I(1)))
        val x78 = x77.asInstanceOf[Clo].fun(valueOf(List(x76)))
        x78
      } else {
        B(false)
      }
      x80: Value
    }
    val x82 = Clo(x69)
    x44 = x82
    val x84 = x82.asInstanceOf[Clo].fun(valueOf(List(x43)))
    x84
  }
}
/*****************************************
End of Generated Code
*******************************************/
