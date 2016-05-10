package compile.analysis

import compile.cfg._
import compile.symboltables.{SymbolTable}
import compile.tac.ThreeAddressCode._
import scala.collection.mutable.{ArrayBuffer}
import compile.util.Util.dprintln

case class Web (id: Int, start: Int, end: Int, use: Int){
  var register : String = null

  override def toString : String = {
    return "%s -> start: %d, end: %d, use: %d".format(id, start, end, use)
  }
}

object Web {

  //returns Map[(String, SymbolTable), List[Web]]
  def getWebInBB(bb : NormalBB, genie : WebGenie) = {
    // We start with an empty map of webs
    val table = bb.symbolTable
    var webs = Map.empty[(String, SymbolTable), List[Web]]
    var instrNum = 1
    for(instr <- bb.instrs) {
      instr match {
        case t:TacBinOp => {
          webs = useWebPerTac(webs, t, table, instrNum, genie)
          webs = defWebPerTac(webs, t, table, instrNum, genie)
        }
        case t:TacUnOp => {
          webs = useWebPerTac(webs, t, table, instrNum, genie)
          webs = defWebPerTac(webs, t, table, instrNum, genie)
        }
        case t:TacCopy => {
          webs = useWebPerTac(webs, t, table, instrNum, genie)
          webs = defWebPerTac(webs, t, table, instrNum, genie)
        }
        case t:TacCopyInt => {
          webs = defWebPerTac(webs, t, table, instrNum, genie)
        }
        case t:TacCopyBoolean => {
          webs = defWebPerTac(webs, t, table, instrNum, genie)
        }
        case t:TacIf => {
          webs = useWebPerTac(webs, t, table, instrNum, genie)
        }
        case t:TacIfFalse => {
          webs = useWebPerTac(webs, t, table, instrNum, genie)
        }
        case t:TacReturnValue => {
          webs = useWebPerTac(webs, t, table, instrNum, genie)
        }
        case t:TacArrayLeft => {
          webs = useWebPerTac(webs, t, table, instrNum, genie)
          webs = defWebPerTac(webs, t, table, instrNum, genie)
        }
        case t:TacArrayRight => {
          webs = useWebPerTac(webs, t, table, instrNum, genie)
          webs = defWebPerTac(webs, t, table, instrNum, genie)
        }
        case t:TacMethodCallExpr => {
          webs = useWebPerTac(webs, t, table, instrNum, genie)
          webs = defWebPerTac(webs, t, table, instrNum, genie)
        }
        case t: TacMethodCallStmt => {
          webs = useWebPerTac(webs, t, table, instrNum, genie)
        }
        case _ => {
        }
      }

      instrNum += 1
    }

    bb.webs = webs
  }

  def useWebPerTac(webs: Map[(String, SymbolTable), List[Web]], tac : Tac, table : SymbolTable, instrNum : Int, genie : WebGenie) : Map[(String, SymbolTable), List[Web]] = {
    var usedSet : Set[(String, SymbolTable)] = DCE.convertTacToUsedVarSet(tac, table)

    for ((s,t) <- usedSet) { // There should only be up to two items
      var weblist = webs.get((s,t)) match {
        case Some(l) => {
          // Below is assumed that we are doing per block web generation, but it is not correct for inter-block web generation
          val currentWeb = l.last
          l.dropRight(1) :+ Web(currentWeb.id, currentWeb.start, instrNum - currentWeb.start + 1, currentWeb.use + 1)
        }
        // No match found; for now we are doing intra block web creation
        // So treat this as if we're making a new web
        case None => {
          List(Web(genie.generateWebNumber(), instrNum, instrNum, 0))
        }
      }
    }


    return webs
  }

  def defWebPerTac(webs: Map[(String, SymbolTable), List[Web]], tac : Tac, table : SymbolTable, instrNum : Int, genie : WebGenie) : Map[(String, SymbolTable), List[Web]] = {
    var defSet : Set[(String, SymbolTable)] = DCE.convertTacToDefVarSet(tac, table)

    for ((s,t) <- defSet) { // There should only really be one item
      var weblist = webs.get((s,t)) match {
        case Some(l) => {
          // Below is assumed that we are doing per block web generation, but it is not correct for inter-block web generation
          val currentWeb = l.last
          l.dropRight(1) :+ Web(currentWeb.id, currentWeb.start, instrNum - currentWeb.start + 1, currentWeb.use + 1)
        }
        case None => {
          // No match found
          // Create a new web
          List(Web(genie.generateWebNumber(), instrNum, instrNum, 0) )
        }
      }
    }

    return webs
  }

}
