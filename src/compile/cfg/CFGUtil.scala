package compile.cfg

import compile.exceptionhandling.{NotForIfWhileStmtException, NullElseBlockException}
import compile.tac.ThreeAddressCode.{TacStringLiteral, Tac}
import compile.symboltables.SymbolTable
import compile.tac.{AsmGen}
import compile.util.Util.dprintln

import scala.collection.mutable

object CFGUtil {
  // TODO Remove NOP, check new parent symbol table call didn't affect anything
  // Make sure that on every call to genBlockBB, check if end BB of block already has a child or is null
  // TODO remove Noops, efficient use of constant zero
  // TODO make sure parent <-> child relations are all correct

  def getStringLiteralTacs(bb : NormalBB, doNotTraverseBBs : List[String]) : List[TacStringLiteral] = {
    var currentBB : NormalBB = bb
    var slTacs : List[TacStringLiteral] = List()

    while (currentBB != null) {
      for (tac <- currentBB.instrs) {
        if(tac.isInstanceOf[TacStringLiteral]) {
          slTacs = slTacs :+ tac.asInstanceOf[TacStringLiteral]
          currentBB.instrs -= tac
        }
      }

      if (currentBB.isInstanceOf[BranchBB]) {
        val BBB : BranchBB = currentBB.asInstanceOf[BranchBB]
        if(BBB.child_else != null && !doNotTraverseBBs.contains(BBB.child_else.id)) {
          if(BBB.preincrement == null && BBB.whilestart == null) {
            // Must be if statement
            slTacs = slTacs ::: getStringLiteralTacs(BBB.child_else, doNotTraverseBBs :+ BBB.merge.id)
          } else if (BBB.preincrement == null) {
            // Must be while statement
            slTacs = slTacs ::: getStringLiteralTacs(BBB.child_else, doNotTraverseBBs :+ BBB.merge.id :+ BBB.whilestart.id)
          } else if (BBB.whilestart == null) {
            slTacs = slTacs ::: getStringLiteralTacs(BBB.child_else, doNotTraverseBBs :+ BBB.merge.id :+ BBB.preincrement.id)
            slTacs = slTacs ::: getStringLiteralTacs(BBB.preincrement, doNotTraverseBBs :+ BBB.merge.id :+ BBB.forstart.id)
          } else {
            throw new NotForIfWhileStmtException("Oh no.")
          }
        } else if (BBB.child_else == null) {
          throw new NullElseBlockException("Uh oh! For some reason the compiler detected a child else basic block that was null!")
        }

      }

      if(currentBB.child != null && doNotTraverseBBs.contains(currentBB.child.id)) {
        currentBB = null
      } else {
        currentBB = currentBB.child
      }
    }

    return slTacs
  }

  // TODO : Untested
  def compressCfg(bb: NormalBB, doNotTraverseBBs : List[String], updateMap : Map[String, String]) : Map[String, String] = {
    // Returns map of removed block -> new block
    var currentBB : NormalBB = bb
    var map : Map[String, String] = updateMap // This map keeps track of block merges
    var doNotTraverseBBList : List[String] = doNotTraverseBBs

    while(currentBB != null && currentBB.child != null) {
      //dprintln("current bb: %s, child: %s".format(currentBB, currentBB.child))

      // Fixing child
      while (map.keySet.exists(_ == currentBB.child.id)) {
        //dprintln("fixing child! was %s now is %s".format(currentBB.child.id, map(currentBB.child.id)))
        currentBB.child = BasicBlockGenie.idToBBReference(map(currentBB.child.id))
      }

      if (currentBB.isInstanceOf[BranchBB]) {
        val BBB : BranchBB = currentBB.asInstanceOf[BranchBB]
        if(BBB.child_else != null && !doNotTraverseBBList.contains(BBB.child_else.id)) {
          if(BBB.preincrement == null && BBB.whilestart == null) {
            // Must be if statement
            // TODO update the references
            updateBranchBBReferences(BBB, map)
            map = mergeCompressMaps(map, compressCfg(BBB.child_else, doNotTraverseBBList :+ BBB.merge.id, map))
            updateBranchBBReferences(BBB, map)
          } else if (BBB.preincrement == null) {
            // Must be while statement
            map = mergeCompressMaps(map, compressCfg(BBB.child_else, doNotTraverseBBList :+ BBB.merge.id :+ BBB.whilestart.id, map))
            updateBranchBBReferences(BBB, map)

          } else if (BBB.whilestart == null) {
            // Must be (?) for statement
            //dprintln("for reached.. attempting recursive compression")

            map = mergeCompressMaps(map, compressCfg(BBB.child_else, doNotTraverseBBList :+ BBB.merge.id :+ BBB.preincrement.id, map))
            //dprintln("for reached.. first recursion complete")

            updateBranchBBReferences(BBB, map)
            map = mergeCompressMaps(map, compressCfg(BBB.preincrement, doNotTraverseBBList :+ BBB.merge.id :+ BBB.forstart.id, map))
            //dprintln("for reached.. second recursion complete")

            updateBranchBBReferences(BBB, map)

          } else {
            throw new NotForIfWhileStmtException("Oh no.")
          }
        } else if (BBB.child_else == null) {
          throw new NullElseBlockException("Uh oh! For some reason the compiler detected a child else basic block that was null!")
        }

        currentBB = currentBB.child

      } else { // MethodCallBB, MergeBB, JumpDestBB, or an ordinary NormalBB
               // Otherwise, only merge in ordinary NormalBB children

        if (currentBB.parent != null && currentBB.instrs.size == 0 && isOrdinaryBB(currentBB)) {
          // We really should only be having empty ordinary BB blocks, but the isOrdinaryBB check is there in case
          // If current block is empty, then remove it and connect its parent and child together
          // We already checked that currentBB.child is not null
          if (doNotTraverseBBList.contains(currentBB.child.id)) {
            currentBB = null
          } else {
            // Update the map to reflect block merge
            map = map + {currentBB.id -> currentBB.child.id}

            // Connecting child's parent pointer to the parent
            currentBB.child.parent = currentBB.parent

            // All the messy stuff to deal with connecting the parent's child pointer to the child
            if (currentBB.parent.isInstanceOf[BranchBB]) {
              val BBBParent : BranchBB = currentBB.parent.asInstanceOf[BranchBB]

              // Fixing the child pointer
              if (BBBParent.child_else != null && BBBParent.child_else.id == currentBB.id) {
                BBBParent.child_else = currentBB.child
              } else {
                BBBParent.child = currentBB.child
              }

            } else {
              currentBB.parent.child = currentBB.child
            }

            // Fix the doNotTraverse list
            // This actually shouldn't matter because we wouldn't reach this if this bb was in the list
            if (doNotTraverseBBList.contains(currentBB.id)) {
              doNotTraverseBBList = doNotTraverseBBList.filter(_ != currentBB.id)
              doNotTraverseBBList = doNotTraverseBBList :+ currentBB.child.id
            }

            // Continue compression with the child
            currentBB = currentBB.child
          }

        } else { // Otherwise, merge in any ordinary child basic blocks 
          if (isOrdinaryBB(currentBB.child) && !doNotTraverseBBs.contains(currentBB.child.id)) {
            // Get all the child's tacs and then merge it into ours, only if the symbol tables are the same
            if (currentBB.child.instrs.size > 0 && currentBB.symbolTable == currentBB.child.symbolTable) {
              // Merge tacs
              for (tac <- currentBB.child.instrs) {
                currentBB.instrs += tac
              }
            }

            // If the symbol tables are the same, or the child has no tacs, then remove the child and connect to its child
            if (currentBB.child.instrs.size == 0 || currentBB.symbolTable == currentBB.child.symbolTable) {
              // Update the map to reflect block merge
              map = map + {currentBB.child.id -> currentBB.id}

              currentBB.child = currentBB.child.child // Update to the new child
              if (currentBB.child != null) {
                // Update the new child's parent pointer
                currentBB.child.parent = currentBB
              }
            } else { // Otherwise, just proceed onto the child
              currentBB = currentBB.child
            }

          } else if (doNotTraverseBBs.contains(currentBB.child.id)) {
            currentBB = null
          } else {
            // Continue compression with the child
            currentBB = currentBB.child
          }
        }
      }
    }
    return map
  }


  def cfgBBs(bb: NormalBB, doNotTraverseBBs : List[String]) : Set[String] = {
    // Returns a set of BB ids that are valid basic blocks in the CFG
    var currentBB : NormalBB = bb
    var set : Set[String] = Set()

    while (currentBB != null) {
      set = set + currentBB.id
      if (currentBB.isInstanceOf[BranchBB]) {
        val BBB : BranchBB = currentBB.asInstanceOf[BranchBB]
        if(BBB.child_else != null && !doNotTraverseBBs.contains(BBB.child_else.id)) {
          if(BBB.preincrement == null && BBB.whilestart == null) {
            // Must be if statement
            set = set | cfgBBs(BBB.child_else, doNotTraverseBBs :+ BBB.merge.id)
          } else if (BBB.preincrement == null) {
            // Must be while statement
            set = set | cfgBBs(BBB.child_else, doNotTraverseBBs :+ BBB.merge.id :+ BBB.whilestart.id)
          } else if (BBB.whilestart == null) {
            set = set | cfgBBs(BBB.child_else, doNotTraverseBBs :+ BBB.merge.id :+ BBB.preincrement.id)
            set = set | cfgBBs(BBB.preincrement, doNotTraverseBBs :+ BBB.merge.id :+ BBB.forstart.id)
          } else {
            throw new NotForIfWhileStmtException("Oh no.")
          }
        } else if (BBB.child_else == null) {
          throw new NullElseBlockException("Uh oh! For some reason the compiler detected a child else basic block that was null!")
        }

      }

      if(currentBB.child != null && doNotTraverseBBs.contains(currentBB.child.id)) {
        currentBB = null
      } else {
        currentBB = currentBB.child
      }
    }
    return set
  }


  def isOrdinaryBB(bb: NormalBB) : Boolean = {
    // Checks that we don't have any of the fancy basic blocks
    return (!bb.isInstanceOf[MethodCallBB]) && (!bb.isInstanceOf[BranchBB]) && (!bb.isInstanceOf[MergeBB]) && (!bb.isInstanceOf[JumpDestBB])
  }

  def cfgToTacs(bb: NormalBB, doNotTraverseBBs : List[String]): List[(Tac, SymbolTable)] = {
    var currentBB : NormalBB = bb
    var tacs : List[(Tac, SymbolTable)] = List()

    while (currentBB != null) {
      if (currentBB.isInstanceOf[MethodCallBB]) {
        val MCBB : MethodCallBB = currentBB.asInstanceOf[MethodCallBB]
        for (tac <- currentBB.instrs) {
          tacs = tacs :+ (tac, MCBB.symbolTable)
        }

      } else if (currentBB.isInstanceOf[BranchBB]) {
        val BBB : BranchBB = currentBB.asInstanceOf[BranchBB]
        for (tac <- currentBB.instrs) {
          tacs = tacs :+ (tac, BBB.symbolTable)
        }

        if(BBB.child_else != null && !doNotTraverseBBs.contains(BBB.child_else.id)) {
          if(BBB.preincrement == null && BBB.whilestart == null) {
            // Must be if statement
            tacs = tacs ::: cfgToTacs(BBB.child_else, doNotTraverseBBs :+ BBB.merge.id)
          } else if (BBB.preincrement == null) {
            // Must be while statement
            tacs = tacs ::: cfgToTacs(BBB.child_else, doNotTraverseBBs :+ BBB.merge.id :+ BBB.whilestart.id)
          } else if (BBB.whilestart == null) {
            tacs = tacs ::: cfgToTacs(BBB.child_else, doNotTraverseBBs :+ BBB.merge.id :+ BBB.preincrement.id)
            tacs = tacs ::: cfgToTacs(BBB.preincrement, doNotTraverseBBs :+ BBB.merge.id :+ BBB.forstart.id)
          } else {
            throw new NotForIfWhileStmtException("Oh no.")
          }
        } else if (BBB.child_else == null) {
          throw new NullElseBlockException("Uh oh! For some reason the compiler detected a child else basic block that was null!")
        }

      } else if (currentBB.isInstanceOf[MergeBB]) {
        val MBB : MergeBB = currentBB.asInstanceOf[MergeBB]
        for (tac <- currentBB.instrs) {
          tacs = tacs :+ (tac, MBB.symbolTable)
        }

      } else if (currentBB.isInstanceOf[JumpDestBB]) {
        val JDBB : JumpDestBB = currentBB.asInstanceOf[JumpDestBB]
        for (tac <- currentBB.instrs) {
          tacs = tacs :+ (tac, JDBB.symbolTable)
        }

      } else { // Just an ordinary NormalBB otherwise
        for (tac <- currentBB.instrs) {
          tacs = tacs :+ (tac, currentBB.symbolTable)
        }
      }

      if(currentBB.child != null && doNotTraverseBBs.contains(currentBB.child.id)) {
        currentBB = null
      } else {
        currentBB = currentBB.child
      }
    }

    return tacs
  }

  def tacsToAsm(tacs: List[(Tac, SymbolTable)]): List[String] = {
    var asm : List[String] = List()

    for((tac, symbolTable) <- tacs) {
      asm = asm ::: AsmGen.asmGen(tac, symbolTable)
      asm = asm :+ "\n"
    }

    return asm
  }

  def mapToDot(map : Map[String, Set[String]], printParents : Boolean = true, printDataflow : Boolean = false) : List[String] = {
    var dot : List[String] = List()
    dot = dot :+ "digraph G {\n"

    for ((parent,children) <- map) {
      val parentBB = BasicBlockGenie.idToBBReference(parent)
      val instrs = parentBB.instrs

      // Don't actually need to initialize cse/DCE strs here, but since these are initialized to empty doesn't hurt
      val cseInStr = parentBB.cseIn.mkString("\\l,")
      val cseOutStr = parentBB.cseOut.mkString("\\l,")

      var dceInStr = "{"
      for((variable, symbolTable) <- parentBB.dceIn) {
        //dceInStr += "(" + variable + "," + symbolTable.hashCode().toString + ")"
        dceInStr += variable + ","
      }
      dceInStr += "}"

      var dceOutStr = "{"
      for((variable, symbolTable) <- parentBB.dceOut) {
        if(variable == null) {
        } else if (symbolTable == null) {
        }
        // dceOutStr += "(" + variable + "," + symbolTable.hashCode().toString + ")"
        dceInStr += variable + ","
      }
      dceOutStr += "}"

      var webStr = "{"
      for((key, listDefUses) <- parentBB.webs) {
        webStr += key._1 + "->" + listDefUses.mkString(",") + ","
      }
      webStr += "}"

      if(parentBB.isInstanceOf[BranchBB]) {
        val parentBranchBB = parentBB.asInstanceOf[BranchBB]
        val merge = if(parentBranchBB.merge == null) "" else parentBranchBB.merge.id
        val preinc = if(parentBranchBB.preincrement == null) "" else parentBranchBB.preincrement.id
        val ws = if(parentBranchBB.whilestart == null) "" else parentBranchBB.whilestart.id
        val forstart = if(parentBranchBB.forstart == null) "" else parentBranchBB.forstart.id
        val child = if(parentBranchBB.child == null) "" else parentBranchBB.child.id
        val child_else = if(parentBranchBB.child_else == null) "" else parentBranchBB.child_else.id
        if(printDataflow) {
          //                                                                          web
          dot = dot :+ "\t%s [shape=box,label=\"%s\\n%s\\n\\n%s\\n%s\\n\\n%s\\n%s\\n\\n%s\\n\\n%s\\n%s\\n%s\\n%s\\n%s\\n%s\\n\\n%s\"];\n".format(
            parent.substring(1),
            parent.substring(1),
            "type: " + getBBType(parentBB),
            "cseIn: " + cseInStr,
            "cseOut: " + cseOutStr,
            "dceIn: " + dceInStr,
            "dceOut: " + dceOutStr,
            "web: " + webStr,
            "merge: " + merge,
            "preinc: " + preinc,
            "whilest: " + ws,
            "forstart: " + forstart,
            "child: " + child,
            "child else: " + child_else,
            instrs.mkString("\\n")
          )
        } else {
          dot = dot :+ "\t%s [shape=box,label=\"%s\\n\\n%s\\n%s\\n%s\\n%s\\n%s\\n%s\\n%s\\n%s\\n%s\\n\\n%s\"];\n".format(
            parent.substring(1),
            parent.substring(1),
            "type: " + getBBType(parentBB),
            "merge: " + merge,
            "preinc: " + preinc,
            "whilest: " + ws,
            "forstart: " + forstart,
            "child: " + child,
            "child else: " + child_else,
            instrs.mkString("\\n")
          )
        }
      } else {
        if(printDataflow) {
          dot = dot :+ "\t%s [shape=box,label=\"%s\\n%s\\n\\n%s\\n%s\\n\\n%s\\n%s\\n\\n%s\\n\\n%s\"];\n".format(
            parent.substring(1),
            parent.substring(1),
            "type: " + getBBType(parentBB),
            "cseIn: " + cseInStr,
            "cseOut: " + cseOutStr,
            "dceIn: " + dceInStr,
            "dceOut: " + dceOutStr,
            "web: " + webStr,
            instrs.mkString("\\n")
          )
        } else {
          dot = dot :+ "\t%s [shape=box,label=\"%s\\n%s\\n\\n%s\"];\n".format(
            parent.substring(1),
            parent.substring(1),
            "type: " + getBBType(parentBB),
            instrs.mkString("\\n")
          )
        }
      }
      for (child <- children) {
        dot = dot :+ "\t%s -> %s;\n".format(parent.substring(1), child.substring(1))
      }
    }

    // graph parent relations now
    if(printParents) {
      for((id, child) <- BasicBlockGenie.idToBBReference) {
        val parents = child.getParents()
        for (parent <- parents) {
          if(parent != null) {
            dot = dot :+ "\t%s -> %s [color = red];\n".format(child.id.substring(1), parent.id.substring(1))
          }
        }
      }
    }


    dot = dot :+ "}\n"
    return dot
  }

  def cfgToMap(bb: NormalBB, doNotTraverseBBs : List[String]) : Map[String,Set[String]] = {
    // This returns a map of bb ids -> set of bb ids
    var currentBB : NormalBB = bb
    var map : Map[String, Set[String]] = Map()

    while (currentBB != null) {
      if (currentBB.isInstanceOf[MethodCallBB]) {
        val MCBB : MethodCallBB = currentBB.asInstanceOf[MethodCallBB]
        map = addNodeToMap(map, MCBB)
      } else if (currentBB.isInstanceOf[BranchBB]) {

        val BBB : BranchBB = currentBB.asInstanceOf[BranchBB]
        map = addNodeToMap(map, BBB)

        if(BBB.child_else != null && !doNotTraverseBBs.contains(BBB.child_else.id)) {
          map = addChildElseNodeToMap(map, BBB)
          if(BBB.preincrement == null && BBB.whilestart == null) {
            // Must be if statement
            val elseMap : Map[String,Set[String]] = cfgToMap(BBB.child_else, doNotTraverseBBs :+ BBB.merge.id)
            map = mergeMaps(map, elseMap)
          } else if (BBB.preincrement == null) {
            // Must be while statement
            val elseMap : Map[String,Set[String]] = cfgToMap(BBB.child_else, doNotTraverseBBs :+ BBB.merge.id :+ BBB.whilestart.id)
            map = mergeMaps(map, elseMap)
          } else if (BBB.whilestart == null) {
            // Must be for statment
            val elseMap1 : Map[String, Set[String]] = cfgToMap(BBB.child_else, doNotTraverseBBs :+ BBB.merge.id :+ BBB.preincrement.id)
            val elseMap2 : Map[String, Set[String]] = cfgToMap(BBB.preincrement, doNotTraverseBBs :+ BBB.merge.id :+ BBB.forstart.id)

            map = mergeMaps(map, elseMap1)
            map = mergeMaps(map, elseMap2)
          } else {
            throw new NotForIfWhileStmtException("Oh no.")
          }
        } else if (BBB.child_else == null) {
          throw new NullElseBlockException("Uh oh! For some reason the compiler detected a child else basic block that was null!")
        }

      } else if (currentBB.isInstanceOf[MergeBB]) {
        val MBB : MergeBB = currentBB.asInstanceOf[MergeBB]
        map = addNodeToMap(map, MBB)
      } else if (currentBB.isInstanceOf[JumpDestBB]) {
        val JDBB : JumpDestBB = currentBB.asInstanceOf[JumpDestBB]
        map = addNodeToMap(map, JDBB)
      } else { // Just an ordinary NormalBB otherwise
        map = addNodeToMap(map, currentBB)
      }

      if(currentBB.child != null && doNotTraverseBBs.contains(currentBB.child.id)) {
        currentBB = null
      } else {
        currentBB = currentBB.child
      }

    }

    return map
  }

  def addChildElseNodeToMap(map:Map[String,Set[String]], node:BranchBB) : Map[String,Set[String]] = {
    val parent : String = node.id
    var set : Set[String] =
      map.get(parent) match {
        case Some(s) => s
        case None => Set()
      }

    if (node.child_else != null) {
      val child : String = node.child_else.id
      set += child
    }
    return (map - parent) + {parent -> set}
  }

  def addNodeToMap(map:Map[String,Set[String]], node:NormalBB) : Map[String,Set[String]] = {   
    val parent : String = node.id
    var set : Set[String] =
      map.get(parent) match {
        case Some(s) => s
        case None => Set()
      }

    if (node.child != null) {
      val child : String = node.child.id
      set += child
    } 
      return (map - parent) + {parent -> set}
  }

  def mergeMaps(map1:Map[String, Set[String]], map2:Map[String, Set[String]]) : Map[String, Set[String]] = {
    var map : Map[String, Set[String]] = map1

    for ((k,v) <- map2) {
      var set : Set[String] =
        map.get(k) match {
          case Some(s) => s
          case None => Set()
        }
      set = set | v
      map = (map - k) + {k -> set}
    }

    return map
  }

  def mergeCompressMaps(map1:Map[String, String], map2:Map[String, String]) : Map[String, String] = {
    var map : Map[String, String] = map1

    for ((k,v) <- map2) {
      val check : String =
        map.get(k) match {
          case Some(s) => s
          case None => ""
        }

      if (check != "" && check != v) {
        dprintln("Error: Found key %s has value %s in map1 and value %s in map2".format(k, check, v))
      }
      map = map + {k -> v}
    }

    return map
  }

  def updateBranchBBReferences(bb:BranchBB, map: Map[String, String]) = {

    // Fixing merge
    while (bb.merge != null && map.keySet.exists(_ == bb.merge.id)) {
      val id : String = map(bb.merge.id)
      bb.merge = BasicBlockGenie.idToBBReference(map(bb.merge.id))
      if (bb.merge == null) {
        dprintln("Error: Tried to update merge but got null for id %s".format(id))
      }
    }

    // Fixing preinc
    while (bb.preincrement != null && map.keySet.exists(_ == bb.preincrement.id)) {
      var id : String = map(bb.preincrement.id)
      bb.preincrement = BasicBlockGenie.idToBBReference(map(bb.preincrement.id))
      if (bb.merge == null) {
        dprintln("Error: Tried to update preincrement but got null for id %s".format(id))
      }
    }

    // Fixing whilestart
    while (bb.whilestart != null && map.keySet.exists(_ == bb.whilestart.id)) {
      var id : String = map(bb.whilestart.id)
      bb.whilestart = BasicBlockGenie.idToBBReference(map(bb.whilestart.id))
      if (bb.whilestart == null) {
        dprintln("Error: Tried to update preincrement but got null for id %s".format(id))
      }
    }

    // Fixing forstart
    while (bb.forstart != null && map.keySet.exists(_ == bb.forstart.id)) {
      var id : String = map(bb.forstart.id)
      bb.forstart = BasicBlockGenie.idToBBReference(map(bb.forstart.id))
      if (bb.forstart == null) {
        dprintln("Error: Tried to update forstart but got null for id %s".format(id))
      }
    }

  }

  def getBBType(bb:NormalBB) : String = {
    if (bb.isInstanceOf[MethodCallBB]) {
      return "MethodCallBB"
    } else if (bb.isInstanceOf[BranchBB]) {
      return "BranchBB"
    } else if (bb.isInstanceOf[MergeBB]) {
      return "MergeBB"
    } else if (bb.isInstanceOf[JumpDestBB]) {
      return "JumpDestBB"
    } else {
      return "NormalBB"
    }
  }

  def setParentBasedOnChildPointers(): Unit = {
    // Clear all the parent pointers for each block
    for((id, parent) <- BasicBlockGenie.idToBBReference) {
      val children = parent.getChildren()
      for (child <- children) {
        if(child != null) {
          if(child.isInstanceOf[JumpDestBB]) {
            val childJD = child.asInstanceOf[JumpDestBB]
            childJD.jmpParents.clear()
            childJD.parent = null
          } else if (child.isInstanceOf[MergeBB]) {
            val childM = child.asInstanceOf[MergeBB]
            childM.parent_else = null
            childM.parent = null
          } else {
            child.parent = null
          }
        }
      }
    }

    // Set the parent pointers for each block
    for((id, parent) <- BasicBlockGenie.idToBBReference) {
      val children = parent.getChildren()
      for (child <- children) {
        if(child != null) {
          if (child.isInstanceOf[JumpDestBB]) {
            val childJD = child.asInstanceOf[JumpDestBB]
            if (childJD.parent == null) {
              childJD.parent = parent
            } else {
              childJD.jmpParents.append(parent)
            }
          } else if (child.isInstanceOf[MergeBB]) {
            val childM = child.asInstanceOf[MergeBB]
            if (childM.parent_else == null) {
              childM.parent_else = parent
            } else {
              childM.parent = parent
            }
          } else {
            child.parent = parent
          }
        }
      }
    }
  }


}
