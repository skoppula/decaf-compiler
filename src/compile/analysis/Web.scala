package compile.analysis

import compile.cfg._
import compile.exceptionhandling.NothingThatCanBeReplacedException
import compile.symboltables.{SymbolTable}
import compile.tac.TempVariableGenie
import compile.tac.ThreeAddressCode._
import scala.collection.mutable.{ArrayBuffer}
import compile.util.Util.dprintln
import scala.collection.mutable

case class Web (id: Int, start: Int, end: Int, use: Int){
  var register : String = null

  override def toString : String = {
    return "(%s, %d, %d, %d)".format(id, start, end, use)
  }
}

object Web {

  def getWebFromId(webs : Map[(String, SymbolTable), List[Web]], id : Int) : ((String, SymbolTable), Web) = {
    for ((key, lst) <- webs) {
      for(web <- lst) {
        if(web.id == id) {
          return (key, web)
        }
      }
    }
    return (null, null)
  }

  def replaceTacWithReg(instr : Tac, varToReplace : String, reg : String) : Tac = {
    instr match {
      case t:TacBinOp => {
        if(t.addr1 == varToReplace) {
          return new TacBinOp(t.id, reg, t.addr2, t.op, t.addr3)
        } else if(t.addr2 == varToReplace) {
          return new TacBinOp(t.id, t.addr1, reg, t.op, t.addr3)
        } else if(t.addr3 == varToReplace) {
          return new TacBinOp(t.id, t.addr1, t.addr2, t.op, reg)
        } else {
          throw new NothingThatCanBeReplacedException("binop")
        }
      }
      case t:TacUnOp => {
        if(t.addr1 == varToReplace) {
          return new TacUnOp(t.id, reg, t.op, t.addr2)
        } else if(t.addr2 == varToReplace) {
          return new TacUnOp(t.id, t.addr1, t.op, reg)
        } else {
          throw new NothingThatCanBeReplacedException("unop")
        }
      }
      case t:TacCopy => {
        if(t.addr1 == varToReplace) {
          return new TacCopy(t.id, reg, t.addr2)
        } else if(t.addr2 == varToReplace) {
          return new TacCopy(t.id, t.addr1, reg)
        } else {
          throw new NothingThatCanBeReplacedException("copy")
        }
      }
      case t:TacCopyInt => {
        if(t.addr1 == varToReplace) {
          return new TacCopyInt(t.id, reg, t.int)
        } else {
          throw new NothingThatCanBeReplacedException("taccopyint")
        }
      }
      case t:TacCopyBoolean => {
        if(t.addr1 == varToReplace) {
          return new TacCopyBoolean(t.id, reg, t.bool)
        } else {
          throw new NothingThatCanBeReplacedException("taccopyint")
        }
      }
      case t:TacIf => {
        if(t.addr1 == varToReplace) {
          return new TacIf(t.id, reg, t.label)
        } else {
          throw new NothingThatCanBeReplacedException("tacif")
        }
      }
      case t:TacIfFalse => {
        if(t.addr1 == varToReplace) {
          return new TacIfFalse(t.id, reg, t.label)
        } else {
          throw new NothingThatCanBeReplacedException("taciffalse")
        }
      }
      case t:TacReturnValue => {
        if(t.addr1 == varToReplace) {
          return new TacReturnValue(t.id, reg)
        } else {
          throw new NothingThatCanBeReplacedException("tacrv")
        }
      }
      case t:TacArrayLeft => {
        if(t.addr1 == varToReplace) {
          return new TacArrayLeft(t.id, reg, t.index, t.addr2)
        } else if(t.index == varToReplace) {
          return new TacArrayLeft(t.id, t.index, reg, t.addr2)
        } else if(t.addr2 == varToReplace) {
          return new TacArrayLeft(t.id, t.addr1, t.index, reg)
        } else {
          throw new NothingThatCanBeReplacedException("arrayleft")
        }
      }
      case t:TacArrayRight => {
        if(t.addr1 == varToReplace) {
          return new TacArrayRight(t.id, reg, t.addr2, t.index)
        } else if(t.index == varToReplace) {
          return new TacArrayRight(t.id, t.index, t.addr2, reg)
        } else if(t.addr2 == varToReplace) {
          return new TacArrayRight(t.id, t.addr1, reg, t.index)
        } else {
          throw new NothingThatCanBeReplacedException("arrayright")
        }
      }
      case t:TacMethodCallExpr => {
        if(t.addr1 == varToReplace) {
          return new TacMethodCallExpr(t.id, reg, t.method, t.args)
        } else if(t.args.contains(t.addr1)) {
          var newArgList = List[String]()
          for(arg <- t.args) {
            if(arg == varToReplace) {
              newArgList = newArgList :+ reg
            } else {
              newArgList = newArgList :+ arg
            }
          }
          return new TacMethodCallExpr(t.id, t.addr1, t.method, newArgList)
        }
        } else {
          throw new NothingThatCanBeReplacedException("methodcallexpr")
        }
      }
      case t: TacMethodCallStmt => {
        var newArgList = List[String]()
        for(arg <- t.args) {
          if(arg == varToReplace) {
            newArgList = newArgList :+ reg
          } else {
            newArgList = newArgList :+ arg
          }
        }
        return new TacMethodCallStmt(t.id, t.method, newArgList)
      }
      case _ => {
      }
    return null
  }

  def updateWithReg(instrs : mutable.ArrayBuffer[Tac], varToReplace : String, reg : String) : mutable.ArrayBuffer[Tac] = {
    val newInstrs = mutable.ArrayBuffer.empty[Tac]
    for(instr <- instrs) {
      newInstrs.append(replaceTacWithReg(instr, varToReplace, reg))
    }
    return newInstrs
  }

  def updatePosition(position : Int , startPos : Int, endPos : Int) : Int = {
    if(position > endPos) {
      return position + 2
    } else if (position == endPos) {
      return position
    } else if (position == startPos) {
      return position
    } else if(position > startPos) {
        return position + 1
    } else {
      return position
    }
  }

  def shiftAllWebPositions(bb : NormalBB, startPos : Int, endPos : Int) {
    var newWebs = bb.webs
    for ((key, lst) <- bb.webs) {
      var currList = List[(Int, Int, Int, Int)]()
      for(defUse <- lst) {
        val newEnd : Int = updatePosition(defUse.end, startPos, endPos)
        val newStart : Int = updatePosition(defUse.start, startPos, endPos)
        currList = currList :+ (defUse.id, newStart, newEnd, defUse.use)
      }
      newWebs = newWebs.-(key) + {key -> currList}
    }
    bb.webs = newWebs
  }

  def replaceTacsWithRegTacs(bb : NormalBB, regMapping : Map[Int, String], tempGenie: TempVariableGenie) = {
    for((webID, reg) <- regMapping) {
      val ((varName, varST), webToUpdate) = getWebFromId(bb.webs, webID)
      val startPos = webToUpdate.start
      val endPos = webToUpdate.end
      val tacCopyIntoReg = new TacCopy(tempGenie.generateTacNumber(), varName, reg)
      val tacCopyOutOfReg = new TacCopy(tempGenie.generateTacNumber(), reg, varName)
      val updatedInstrs = tacCopyIntoReg +: updateWithReg(bb.instrs.slice(startPos, endPos+1), varName, reg) :+ tacCopyOutOfReg
      shiftAllWebPositions(bb, startPos, endPos)

      bb.instrs.clear()
      bb.instrs ++= (bb.instrs.slice(0, startPos) ++ updatedInstrs ++ bb.instrs.slice(endPos+1, bb.instrs.size))
    }
  }

  //returns Map[(String, SymbolTable), List[Web]]
  def getWebInBB(bb : NormalBB, genie : WebGenie) = {
    // We start with an empty map of webs
    val table = bb.symbolTable
    var webs = Map.empty[(String, SymbolTable), List[Web]]
    var instrNum = 1
    for(instr <- bb.instrs) {
      // dprintln("\t" + instr.toString)
      // dprintln("\t" + WebUtil.printWebOut(webs))
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

    // dprintln(WebUtil.printWebOut(webs))
    bb.webs = webs
  }

  def useWebPerTac(webs: Map[(String, SymbolTable), List[Web]], tac : Tac, table : SymbolTable, instrNum : Int, genie : WebGenie) : Map[(String, SymbolTable), List[Web]] = {
    val usedSet : Set[(String, SymbolTable)] = DCE.convertTacToUsedVarSet(tac, table)
    var newWebs = webs

    for ((s,t) <- usedSet) { // There should only be up to two items
      // dprintln("\t\tChecking var" + s)
      val weblist = webs.get((s,t)) match {
        case Some(l) => {
          // dprintln("Case some")
          // Below is assumed that we are doing per block web generation, but it is not correct for inter-block web generation
          val currentWeb = l.last
          l.dropRight(1) :+ Web(currentWeb.id, currentWeb.start, instrNum, currentWeb.use + 1)
        }
        // No match found; for now we are doing intra block web creation
        // So treat this as if we're making a new web
        case None => {
          // dprintln("Case none")
          List(Web(genie.generateWebNumber(), 0, instrNum, 1))
        }
      }
      newWebs = newWebs.-((s,t)) + {(s,t) -> weblist}
    }

    return newWebs
  }

  def defWebPerTac(webs: Map[(String, SymbolTable), List[Web]], tac : Tac, table : SymbolTable, instrNum : Int, genie : WebGenie) : Map[(String, SymbolTable), List[Web]] = {
    val defSet : Set[(String, SymbolTable)] = DCE.convertTacToDefVarSet(tac, table)
    var newWebs = webs

    for ((s,t) <- defSet) { // There should only really be one item
      val weblist = webs.get((s,t)) match {
        case Some(l) => {
          // Below is assumed that we are doing per block web generation, but it is not correct for inter-block web generation
          val currentWeb = l.last
          l :+ Web(genie.generateWebNumber(), instrNum, instrNum, 0)
        }
        case None => {
          // No match found
          // Create a new web
          List(Web(genie.generateWebNumber(), instrNum, instrNum, 0) )
        }
      }
      newWebs = webs.-((s,t)) + {(s,t) -> weblist}
    }

    return newWebs
  }

}
