def checkIrAssignStmt(
    methodsTable: MethodsTable,			// The entirety of the symbol table structure
    scopeStack: mutable.Stack[SymbolTable],	// The current scope
    stmt: IrAssignStmt,				// The current IrNode being processed/type-checked
    genie: ExceptionGenie			// Handler to catch exceptions
  ) : Boolean 
