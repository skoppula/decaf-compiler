package compile.analysis

import compile.tac.OpTypes._
import compile.tac.ThreeAddressCode._
import scala.collection.mutable.{ArrayBuffer}

object BitvectorKey {
  abstract class BitvectorKey
  case class Bvk (op: OpEnumVal, setVars: Set[String], listVars: ArrayBuffer[String]) extends BitvectorKey{}
  case class EmptyBvk() extends BitvectorKey{} 
}
