package lms

import scala.lms.common._
import scala.reflect.SourceContext

trait LMS_Base extends PrimitiveOps with NumericOps with BooleanOps with LiftString with LiftPrimitives with LiftNumeric with LiftBoolean with IfThenElse with Equal with RangeOps with OrderingOps with MiscOps with ArrayOps with StringOps with SeqOps with Functions with While with StaticData with Variables with LiftVariables with ObjectOps {

}

trait LMS_Exp extends LMS_Base with PrimitiveOpsExpOpt with NumericOpsExpOpt with BooleanOpsExp with IfThenElseExpOpt with EqualExpBridgeOpt with RangeOpsExp with OrderingOpsExp with MiscOpsExp with EffectExp with ArrayOpsExpOpt with StringOpsExp with SeqOpsExp with FunctionsRecursiveExp with WhileExp with StaticDataExp with VariablesExpOpt with ObjectOpsExpOpt {

}

trait LMS_Gen extends ScalaGenNumericOps
    with ScalaGenPrimitiveOps with ScalaGenBooleanOps with ScalaGenIfThenElse
    with ScalaGenEqual with ScalaGenRangeOps with ScalaGenOrderingOps
    with ScalaGenMiscOps with ScalaGenArrayOps with ScalaGenStringOps
    with ScalaGenSeqOps with ScalaGenFunctions with ScalaGenWhile
    with ScalaGenStaticData with ScalaGenVariables
    with ScalaGenObjectOps {
  val IR: LMS_Exp

  import IR._

}

trait LMS_Impl extends LMS_Exp { q =>
  val codegen = new LMS_Gen {
    val IR: q.type = q
  }
}
