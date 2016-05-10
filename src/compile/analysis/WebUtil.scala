package compile.analysis

import compile.cfg.{BranchBB, NormalBB}
import compile.symboltables.SymbolTable
import compile.exceptionhandling.{SymbolVariableIsNullException, TempVariableAlreadyExistsInGlobalMapException, NullElseBlockException, NotForIfWhileStmtException}
import compile.tac.{ThreeAddressCode, TempVariableGenie}
import compile.tac.ThreeAddressCode.{TacUnOp, Tac, TacBinOp, TacCopy}
import compile.util.Util.dprintln

import scala.collection.mutable.ArrayBuffer


object WebUtil {
  def printWebOut(web : Map[(String, SymbolTable), List[Web]]): String = {
    var webStr = "{"
    for((key, listDefUses) <- web) {
      webStr += key._1 + "->" + listDefUses.mkString(",") + ","
    }
    webStr += "}"
    return webStr
  }
}
