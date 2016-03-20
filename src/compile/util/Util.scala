package compile.util

import compile.tac.ThreeAddressCode.Tac

import scala.collection.mutable.LinkedHashMap

object Util {
  def combineLinkedHashMaps(lhm1 : LinkedHashMap[Tac, List[String]], lhm2 : LinkedHashMap[Tac, List[String]]) : LinkedHashMap[Tac, List[String]]= {
    val newLHM : LinkedHashMap[Tac, List[String]] = LinkedHashMap.empty[Tac, List[String]]
    for((tac, asm) <- lhm1) {
      newLHM(tac) = asm
    }

    for((tac, asm) <- lhm2) {
      newLHM(tac) = asm
    }

    return newLHM
  }
}
