-- {-# LANGUAGE RankNTypes #-}

module CodeGen_Statements where

import AST
import CodeGen_Declarations
import CodeGen_Expressions
import CodeGen_Helper


-------------------------------------------------------------------------------------------------- Statement Subroutine Generation Master Function


-- generateStmtSr creates a subroutine call and its associated definitions for a single statement.
-- @param the statement and its index wrt to the list of statements collected by the pasrer
-- @dev There is also an optional baseName parameter that can be used to inject a unique prefix into the 
-- subroutine name. An empty string should be passed into the prefix if no prefix is desired.
generateStmtSr :: String -> Integer -> Stmt -> TypeMap -> (String, String)
generateStmtSr optionalPrefix index stmt typeMap = case stmt of

  -- assignment stmt of preinitialized variable
  AssignStmt lValue expr -> 
    let assignSrName = optionalPrefix ++ lValue ++ "_assignment_" ++ show index
    in (genAssignmentSr assignSrName index lValue expr typeMap) 

  _ -> error "Unsupported statement type" 




------------------------------------------------------------------------------------------------- Asssignment Subroutine generation

-- genAssignmentSr generates a subroutine call and definition for the assignment of an expression to a preinitialized
-- variable (within the .bss section). Another subroutine def is generated for the eval of the expression. It is called 
-- within the assignment subroutine. The output of the expr eval is placed into a different regeister depending on its 
-- type. Then it is moved directly into the memory location of the variable.
-- @param [name of subroutine], [index of stmt], [name of variable], [expression to be assigned]
genAssignmentSr :: String -> Integer -> String -> Expr -> TypeMap -> (String, String)
genAssignmentSr assignSrName index lValue expr typeMap = 
  let
    -- Generate expr eval subroutine
    exprEvalSrBaseName = assignSrName ++ "_" ++ lValue ++ "_expr_eval_" ++ show index
    (exprEvalSrDef, exprEvalSrName, _) = genExprEvalSr exprEvalSrBaseName 0 expr typeMap

    -- type-specific move command
    moveCommand = moveInstr_RegToVar (getExprType expr typeMap) lValue

    -- Generate subroutine call and def for assignment of expr to var
    assignSrCall = "\tcall " ++ assignSrName ++ "\n"
    assignmentSrDef = assignSrName ++ ":\n" ++
                      "\tcall " ++ exprEvalSrName ++ "\n" ++    -- call expr eval 
                      moveCommand ++ "\tret\n"                  -- move result into var 

    -- Combine the expr eval and assignment subroutine definitions
    fullSrDef = assignmentSrDef ++ exprEvalSrDef

  in (assignSrCall, fullSrDef)
