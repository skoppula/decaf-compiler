package compile.analysis

import compile.cfg.{BasicBlockGenie, CFGUtil, NormalBB}
import compile.symboltables.SymbolTable
import compile.tac.TempVariableGenie
import compile.tac.ThreeAddressCode.{TacCopyInt, Tac, TacBinOp, TacCopy}
import compile.util.Util.dprintln

import scala.collection.mutable.ArrayBuffer

object ElimCopiesBeforeOp {
  def elim(methodStart: NormalBB, tempGenie : TempVariableGenie) {
    // Removes the Temps and TAC copies that precede an expression such as c = a+b; removes putting a and b into temps
    val bbIDs : Set[String] = CFGUtil.cfgBBs(methodStart, List())
    for(bbID <- bbIDs) {
      elimFromBBBinOp(BasicBlockGenie.idToBBReference(bbID), tempGenie)
      elimFromBBTacCopyIntTacCopy(BasicBlockGenie.idToBBReference(bbID), tempGenie)
      elimFromBBTacCopyIntBinOp(BasicBlockGenie.idToBBReference(bbID), tempGenie)
      elimFromBBTacCopyBinOp(BasicBlockGenie.idToBBReference(bbID), tempGenie)
      elimFromBBTacCopyIntBinOp(BasicBlockGenie.idToBBReference(bbID), tempGenie)
    }
  }

  def elimFromBBTacCopyBinOp(bb : NormalBB, tempGenie : TempVariableGenie): Unit = {
    /*
    optimization for when:
    TacCopy(T1,B)
    TacBinOp(A,T1,C)
    ->
    TacBinOp(A,B,C)
     */
    val num_instrs = bb.instrs.size
    if(num_instrs < 2) return
    else {
      val newInstrs = ArrayBuffer.empty[Tac]

      var i = 0
      while(i < num_instrs) {
        if(i < num_instrs - 1) {
          val instr1 = bb.instrs(i)
          val instr2 = bb.instrs(i + 1)
          if (instr1.isInstanceOf[TacCopy] && instr2.isInstanceOf[TacBinOp]) {

            val instr1TC = instr1.asInstanceOf[TacCopy]
            val instr2BO = instr2.asInstanceOf[TacBinOp]
            val instr1_addr1 = instr1TC.addr1
            val instr1_addr2 = instr1TC.addr2
            val instr2_addr2 = instr2BO.addr2
            val instr2_addr3 = instr2BO.addr3

            // TODO maybe should check that temps aren't used again???
            val varAndST1 = DCE.getSymbolAndTable(instr1_addr1, bb.symbolTable)

            var tempIsUsed = false
            for(tac <- bb.instrs.slice(i+2,num_instrs)) {
              val uses = DCE.usePerTac(Set.empty[(String, SymbolTable)], tac, bb.symbolTable)
              val used_in_tac1 = uses.contains(varAndST1)
              tempIsUsed = tempIsUsed || used_in_tac1
            }

            if(bb.dceOut.contains(varAndST1) || tempIsUsed) {
              newInstrs.append(bb.instrs(i))
              i += 1
            } else {
              // do the TACCopy substitution
              if (instr1_addr1 == instr2_addr2) {
                newInstrs.append(TacBinOp(tempGenie.generateTacNumber(), instr2BO.addr1, instr1_addr2, instr2BO.op, instr2_addr3))
                i = i + 2
              } else if (instr1_addr1 == instr2_addr3) {
                newInstrs.append(TacBinOp(tempGenie.generateTacNumber(), instr2BO.addr1, instr2_addr2, instr2BO.op, instr1_addr2))
                i = i + 2
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

  def elimFromBBTacCopyIntBinOp(bb : NormalBB, tempGenie : TempVariableGenie): Unit = {
    /*
    optimization for when:
    TacCopyInt(T1,0)
    TacBinOp(A,T1,C)
    ->
    TacBinOp(A,.C0, C)
     */
    val num_instrs = bb.instrs.size
    if(num_instrs < 2) return
    else {
      val newInstrs = ArrayBuffer.empty[Tac]

      var i = 0
      while(i < num_instrs) {
        if(i < num_instrs - 1) {
          val instr1 = bb.instrs(i)
          val instr2 = bb.instrs(i + 1)
          if (instr1.isInstanceOf[TacCopyInt] && instr2.isInstanceOf[TacBinOp]) {

            val instr1TC = instr1.asInstanceOf[TacCopyInt]
            val instr2BO = instr2.asInstanceOf[TacBinOp]
            val instr1_addr1 = instr1TC.addr1
            val instr1_num = instr1TC.int
            val instr2_addr2 = instr2BO.addr2
            val instr2_addr3 = instr2BO.addr3

            // TODO maybe should check that temps aren't used again???
            val varAndST1 = DCE.getSymbolAndTable(instr1_addr1, bb.symbolTable)

            var tempIsUsed = false
            for(tac <- bb.instrs.slice(i+2,num_instrs)) {
              val uses = DCE.usePerTac(Set.empty[(String, SymbolTable)], tac, bb.symbolTable)
              val used_in_tac1 = uses.contains(varAndST1)
              tempIsUsed = tempIsUsed || used_in_tac1
            }

            if(bb.dceOut.contains(varAndST1) || tempIsUsed) {
              newInstrs.append(bb.instrs(i))
              i += 1
            } else {
              // do the TACCopy substitution
              if (instr1_addr1 == instr2_addr2) {
                newInstrs.append(TacBinOp(tempGenie.generateTacNumber(), instr2BO.addr1, ".C" + instr1_num, instr2BO.op, instr2_addr3))
                i = i + 2
              } else if (instr1_addr1 == instr2_addr3) {
                newInstrs.append(TacBinOp(tempGenie.generateTacNumber(), instr2BO.addr1, instr2_addr2, instr2BO.op, ".C" + instr1_num))
                i = i + 2
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

  def elimFromBBTacCopyIntTacCopy(bb : NormalBB, tempGenie : TempVariableGenie) {
    /*
    optimization for when:
    TacCopyInt(T1,0)
    TacCopy(A,T1)
    ->
    TacCopyInt(A,0)
     */

    val num_instrs = bb.instrs.size
    if(num_instrs < 2) return
    else {
      val newInstrs = ArrayBuffer.empty[Tac]

      var i = 0
      while(i < num_instrs) {
        if(i < num_instrs - 1) {
          val instr1 = bb.instrs(i)
          val instr2 = bb.instrs(i + 1)
          if (instr1.isInstanceOf[TacCopyInt] && instr2.isInstanceOf[TacCopy]) {

            val instr1TCI = instr1.asInstanceOf[TacCopyInt]
            val instr2TC = instr2.asInstanceOf[TacCopy]
            val i1_addr1 = instr1TCI.addr1
            val i1_num = instr1TCI.int
            val i2_addr1 = instr2TC.addr1
            val i2_addr2 = instr2TC.addr2

            // TODO would changes by previous bin op optimization mess up DCE_out? i don't think so...
            val varAndST1 = DCE.getSymbolAndTable(i1_addr1, bb.symbolTable)

            var tempIsUsed = false
            for(tac <- bb.instrs.slice(i+2,num_instrs)) {
              val uses = DCE.usePerTac(Set.empty[(String, SymbolTable)], tac, bb.symbolTable)
              val used_in_tac1 = uses.contains(varAndST1)
              tempIsUsed = tempIsUsed || used_in_tac1
            }

            if(bb.dceOut.contains(varAndST1) || tempIsUsed) {
              newInstrs.append(bb.instrs(i))
              i += 1
            } else {
              // do the TACCopy substitution
              if (i1_addr1 == i2_addr2) {
                newInstrs.append(TacCopyInt(tempGenie.generateTacNumber(), i2_addr1, i1_num))
                i = i + 2
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

            if(bb.dceOut.contains(varAndST1) || bb.dceOut.contains(varAndST2) || tempIsUsed) {
              newInstrs.append(bb.instrs(i))
              i += 1
            } else {
              // do the TACCopy substitution
              if (instr1_addr1 == instr3_addr2 && instr2_addr1 == instr3_addr3) {
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
