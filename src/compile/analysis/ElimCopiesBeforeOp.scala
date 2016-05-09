package compile.analysis

import compile.cfg.{BasicBlockGenie, CFGUtil, NormalBB}
import compile.symboltables.SymbolTable
import compile.tac.TempVariableGenie
import compile.tac.ThreeAddressCode.{Tac, TacBinOp, TacCopy}
import compile.util.Util.dprintln

import scala.collection.mutable.ArrayBuffer

object ElimCopiesBeforeOp {
  def elim(methodStart: NormalBB, tempGenie : TempVariableGenie) {
    // Removes the Temps and TAC copies that precede an expression such as c = a+b; removes putting a and b into temps
    val bbIDs : Set[String] = CFGUtil.cfgBBs(methodStart, List())
    for(bbID <- bbIDs) {
      elimFromBBBinOp(BasicBlockGenie.idToBBReference(bbID), tempGenie)
      elimFromBBUnOp(BasicBlockGenie.idToBBReference(bbID), tempGenie)
    }
  }

  def elimFromBBBinOp(bb : NormalBB, tempGenie : TempVariableGenie) {
    val num_instrs = bb.instrs.size
    if(num_instrs < 3) return
    else {
      val newInstrs = ArrayBuffer.empty[Tac]

      var i = 0
      while(i < num_instrs) {
        if(i < num_instrs - 2) {
          val instr1 = bb.instrs(i)
          val instr2 = bb.instrs(i + 1)
          val instr3 = bb.instrs(i + 2)
          if (instr1.isInstanceOf[TacCopy] && instr2.isInstanceOf[TacCopy] && instr3.isInstanceOf[TacBinOp]) {
            dprintln("\tFound case where I can replace stuff!!!")

            val instr1TC = instr1.asInstanceOf[TacCopy]
            val instr2TC = instr2.asInstanceOf[TacCopy]
            val instr3BO = instr3.asInstanceOf[TacBinOp]
            val instr1_addr1 = instr1TC.addr1
            val instr1_addr2 = instr1TC.addr2
            val instr2_addr1 = instr2TC.addr1
            val instr2_addr2 = instr2TC.addr2
            val instr3_addr2 = instr3BO.addr2
            val instr3_addr3 = instr3BO.addr3

            // TODO maybe should check that temps aren't used again???
            val varAndST1 = DCE.getSymbolAndTable(instr1_addr1, bb.symbolTable)
            val varAndST2 = DCE.getSymbolAndTable(instr2_addr1, bb.symbolTable)

            var tempIsUsed = false
            for(tac <- bb.instrs.slice(i+3,num_instrs)) {
              val uses = DCE.usePerTac(Set.empty[(String, SymbolTable)], tac, bb.symbolTable)
              val used_in_tac1 = uses.contains(varAndST1)
              val used_in_tac2 = uses.contains(varAndST2)
              tempIsUsed = tempIsUsed || used_in_tac1 || used_in_tac2
            }

            dprintln("\tis either of the temps used??" + tempIsUsed)

            if(bb.dceOut.contains(varAndST1) || bb.dceOut.contains(varAndST2) || tempIsUsed) {
              newInstrs.append(bb.instrs(i))
              i += 1
            } else {
              // do the TACCopy substitution
              if (instr1_addr1 == instr3_addr2 && instr2_addr1 == instr3_addr3) {
                dprintln("\taddresses check out!!!")
                if(instr1_addr1 == instr2_addr2) {
                  /*
                  case when:
                  TacCopy(A,B)
                  TacCopy(C,A)
                  BinOp(D,A,C) -> BinOp(D, B, B)
                   */
                  newInstrs.append(TacBinOp(tempGenie.generateTacNumber(), instr3BO.addr1, instr1_addr2, instr3BO.op, instr1_addr2))
                } else {
                  newInstrs.append(TacBinOp(tempGenie.generateTacNumber(), instr3BO.addr1, instr1_addr2, instr3BO.op, instr2_addr2))
                }
                i = i + 3
              } else if (instr2_addr1 == instr3_addr2 && instr1_addr1 == instr3_addr2) {
                dprintln("\taddresses check out!!!")
                if(instr1_addr1 == instr2_addr2) {
                  /*
                  case when:
                  TacCopy(A,B)
                  TacCopy(C,A)
                  BinOp(D,C,A) -> BinOp(D, B, B)
                   */
                  newInstrs.append(TacBinOp(tempGenie.generateTacNumber(), instr3BO.addr1, instr1_addr2, instr3BO.op, instr1_addr2))
                } else {
                  newInstrs.append(TacBinOp(tempGenie.generateTacNumber(), instr3BO.addr1, instr2_addr2, instr3BO.op, instr1_addr2))
                }
                i = i + 3
              } else {
                newInstrs.append(bb.instrs(i))
                i += 1
              }
            }

          } else {
            newInstrs.append(bb.instrs(i))
            i += 1
          }
        } else {
          newInstrs.append(bb.instrs(i))
          i += 1
        }
      }

      bb.instrs.clear()
      bb.instrs ++= newInstrs
    }
  }
}
