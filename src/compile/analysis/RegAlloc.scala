package compile.analysis

import compile.cfg.{BranchBB, NormalBB}
import compile.symboltables.{MethodsTable, SymbolTable}
import compile.exceptionhandling.{SymbolVariableIsNullException, TempVariableAlreadyExistsInGlobalMapException, NullElseBlockException, NotForIfWhileStmtException}
import compile.tac.{ThreeAddressCode, TempVariableGenie}
import compile.tac.ThreeAddressCode._
import compile.util.Util.dprintln
import scala.collection.mutable

object RegAlloc {

  def assignRegistersToWebs(bb: NormalBB) = {

    val allWebs : mutable.ArrayBuffer[Web] = mutable.ArrayBuffer.empty[Web]

    for((k,webs) <- bb.webs) {
      allWebs ++= webs
    }

    var registers : mutable.ArrayBuffer[(String,Web)] = new mutable.ArrayBuffer[(String,Web)]
    registers += (("reg1",null))
    registers += (("reg2",null))
    registers += (("reg3",null))
    registers += (("reg4",null))

    var regsFilled = 0;

    for(web <- allWebs.sortBy(_.use)){ //change this to use density (.end - .start) / .use


      for(r <- registers){
        r._2 match {
          case w : Web => {
            if(regsFilled >= registers.length){
              if((web.start < w.end && web.start > w.start) || (web.end > w.start && web.end < w.end)){
                //conflict
              }
              else{
                //r._2 = web
                web.register = r._1
                regsFilled += 1;
              }
            }
          }
          case _ => {
            //r._2 = web
            web.register = r._1
            regsFilled += 1;
          }
        }
      }
    }
  }

}
