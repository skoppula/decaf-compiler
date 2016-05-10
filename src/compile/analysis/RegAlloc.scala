package compile.analysis

import compile.cfg.{BranchBB, NormalBB}
import compile.descriptors.ArrayBaseDescriptor
import compile.symboltables.{MethodsTable, SymbolTable}
import compile.exceptionhandling._
import compile.tac.{ThreeAddressCode, TempVariableGenie}
import compile.tac.ThreeAddressCode._
import compile.util.Util.dprintln
import scala.collection.mutable

object RegAlloc {

  def assignRegistersToWebs(bb: NormalBB) = {

    val MIN_USAGE = 4

    val allWebs : mutable.ArrayBuffer[Web] = mutable.ArrayBuffer.empty[Web]

    for((k,webs) <- bb.webs) {
      if(!k._2.lookupID(k._1).isInstanceOf[ArrayBaseDescriptor]) {
        allWebs ++= webs
      }
    }

    var registers : mutable.ArrayBuffer[(String,mutable.ArrayBuffer[Web])] = new mutable.ArrayBuffer[(String,mutable.ArrayBuffer[Web])]
    registers += ((".Rr12",mutable.ArrayBuffer.empty[Web]))
    registers += ((".Rr13",mutable.ArrayBuffer.empty[Web]))
    registers += ((".Rr14",mutable.ArrayBuffer.empty[Web]))
    registers += ((".Rr15",mutable.ArrayBuffer.empty[Web]))

    for(web <- allWebs) {
      web.density = (web.end - web.start) / (0.0 + web.use)
    }

    for(web <- allWebs.filter(w => w.use > MIN_USAGE).sortBy(_.density)){ //change this to use density (.end - .start) / .use
      for(r <- registers){
        if(web.register == null) {
          if(r._2.isEmpty) {
            // Case that register hasn't been assigned anything (null)
            r._2 ++= mutable.ArrayBuffer(web)
            web.register = r._1
            bb.webIDToReg = bb.webIDToReg + {web.id -> web.register}
          } else {
            val pastWebs = r._2
            var conflicts = false
            for(w <- pastWebs) {
              if ((web.start < w.end && web.start > w.start) || (web.end > w.start && web.end < w.end)) {
                conflicts = true
              }
            }
            if(!conflicts){
              pastWebs ++= mutable.ArrayBuffer(web)
              web.register = r._1
              bb.webIDToReg = bb.webIDToReg + {web.id -> web.register}
            }
          }
        }
      }
    }
  }


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
      case t: TacBinOp => {
        if (t.addr1 == varToReplace) {
          return new TacBinOp(t.id, reg, t.addr2, t.op, t.addr3)
        } else if (t.addr2 == varToReplace) {
          return new TacBinOp(t.id, t.addr1, reg, t.op, t.addr3)
        } else if (t.addr3 == varToReplace) {
          return new TacBinOp(t.id, t.addr1, t.addr2, t.op, reg)
        } else {
          return null
          // throw new NothingThatCanBeReplacedException("binop")
        }
      }
      case t: TacUnOp => {
        if (t.addr1 == varToReplace) {
          return new TacUnOp(t.id, reg, t.op, t.addr2)
        } else if (t.addr2 == varToReplace) {
          return new TacUnOp(t.id, t.addr1, t.op, reg)
        } else {
          return null
          // throw new NothingThatCanBeReplacedException("unop")
        }
      }
      case t: TacCopy => {
        if (t.addr1 == varToReplace) {
          return new TacCopy(t.id, reg, t.addr2)
        } else if (t.addr2 == varToReplace) {
          return new TacCopy(t.id, t.addr1, reg)
        } else {
          return null
          // throw new NothingThatCanBeReplacedException("copy")
        }
      }
      case t: TacCopyInt => {
        if (t.addr1 == varToReplace) {
          return new TacCopyInt(t.id, reg, t.int)
        } else {
          return null
          // throw new NothingThatCanBeReplacedException("taccopyint")
        }
      }
      case t: TacCopyBoolean => {
        if (t.addr1 == varToReplace) {
          return new TacCopyBoolean(t.id, reg, t.bool)
        } else {
          return null
          // throw new NothingThatCanBeReplacedException("taccopyint")
        }
      }
      case t: TacIf => {
        if (t.addr1 == varToReplace) {
          return new TacIf(t.id, reg, t.label)
        } else {
          return null
          // throw new NothingThatCanBeReplacedException("tacif")
        }
      }
      case t: TacIfFalse => {
        if (t.addr1 == varToReplace) {
          return new TacIfFalse(t.id, reg, t.label)
        } else {
          return null
          // throw new NothingThatCanBeReplacedException("taciffalse")
        }
      }
      case t: TacReturnValue => {
        if (t.addr1 == varToReplace) {
          return new TacReturnValue(t.id, reg)
        } else {
          return null
          // throw new NothingThatCanBeReplacedException("tacrv")
        }
      }
      case t: TacArrayLeft => {
        if (t.addr1 == varToReplace) {
          return new TacArrayLeft(t.id, reg, t.index, t.addr2)
        } else if (t.index == varToReplace) {
          return new TacArrayLeft(t.id, t.index, reg, t.addr2)
        } else if (t.addr2 == varToReplace) {
          return new TacArrayLeft(t.id, t.addr1, t.index, reg)
        } else {
          return null
          // throw new NothingThatCanBeReplacedException("arrayleft")
        }
      }
      case t: TacArrayRight => {
        if (t.addr1 == varToReplace) {
          return new TacArrayRight(t.id, reg, t.addr2, t.index)
        } else if (t.index == varToReplace) {
          return new TacArrayRight(t.id, t.index, t.addr2, reg)
        } else if (t.addr2 == varToReplace) {
          return new TacArrayRight(t.id, t.addr1, reg, t.index)
        } else {
          return null
          // throw new NothingThatCanBeReplacedException("arrayright")
        }
      }
      case t: TacMethodCallExpr => {
        if (t.addr1 == varToReplace) {
          return new TacMethodCallExpr(t.id, reg, t.method, t.args)
        } else if (t.args.contains(t.addr1)) {
          var newArgList = List[String]()
          for (arg <- t.args) {
            if (arg == varToReplace) {
              newArgList = newArgList :+ reg
            } else {
              newArgList = newArgList :+ arg
            }
          }
          return new TacMethodCallExpr(t.id, t.addr1, t.method, newArgList)
        } else {
          return null
          // throw new NothingThatCanBeReplacedException("methodcallexpr")
        }
      }
      case t: TacMethodCallStmt => {
        var newArgList = List[String]()
        for (arg <- t.args) {
          if (arg == varToReplace) {
            newArgList = newArgList :+ reg
          } else {
            newArgList = newArgList :+ arg
          }
        }
        return new TacMethodCallStmt(t.id, t.method, newArgList)
      }
      case _ => {
      }
    }
    return null
  }

  def updateWithReg(instrs : mutable.ArrayBuffer[Tac], varToReplace : String, reg : String) : mutable.ArrayBuffer[Tac] = {
    val newInstrs = mutable.ArrayBuffer.empty[Tac]
    for(instr <- instrs) {
      val new_tac = replaceTacWithReg(instr, varToReplace, reg)
      if(new_tac == null) {
        newInstrs.append(instr)
      } else {
        newInstrs.append(new_tac)
      }
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
    var newWebs  = bb.webs
    for ((key, lst) <- bb.webs) {
      var currList = List[Web]()
      for(defUse <- lst) {
        val newEnd : Int = updatePosition(defUse.end, startPos, endPos)
        val newStart : Int = updatePosition(defUse.start, startPos, endPos)
        val newWeb = Web(defUse.id, newStart, newEnd, defUse.use)
        newWeb.register = defUse.register
        newWeb.density = defUse.density
        currList = currList :+ newWeb
      }
      newWebs = newWebs - key + {key -> currList}
    }
    bb.webs = newWebs
  }

  def replaceTacsWithRegTacs(bb : NormalBB, tempGenie: TempVariableGenie) = {
    val regMapping : Map[Int, String] = bb.webIDToReg
    for((webID, reg) <- regMapping) {
      val ((varName, varST), webToUpdate) = getWebFromId(bb.webs, webID)
      val startPos = webToUpdate.start
      val endPos = webToUpdate.end
      val tacCopyIntoReg = new TacCopy(tempGenie.generateTacNumber(), reg, varName)
      val tacCopyOutOfReg = new TacCopy(tempGenie.generateTacNumber(), varName, reg)
      val updatedInstrs = tacCopyIntoReg +: updateWithReg(bb.instrs.slice(startPos, endPos+1), varName, reg) :+ tacCopyOutOfReg
      shiftAllWebPositions(bb, startPos, endPos)

      val startInstrs = bb.instrs.slice(0, startPos)
      val endInstrs = bb.instrs.slice(endPos+1, bb.instrs.size)
      bb.instrs.clear()
      bb.instrs ++= (startInstrs ++ updatedInstrs ++ endInstrs)
    }
  }


}
