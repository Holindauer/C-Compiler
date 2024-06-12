-- {-# LANGUAGE RankNTypes #-}

module CodeGen_Statements where

import AST
import CodeGen_Declarations
import CodeGen_Expressions
import CodeGen_Helper

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Hashable


-------------------------------------------------------------------------------------------------- Statement Subroutine Generation Master Function


-- generateStmtSr creates a subroutine call and its associated definitions for a single statement.
-- @param the statement and its index wrt to the list of statements collected by the pasrer
-- @dev There is also an optional baseName parameter that can be used to inject a unique prefix into the 
-- subroutine name. An empty string should be passed into the prefix if no prefix is desired.
generateStmtSr :: String -> Integer -> Stmt -> TypeMap -> HashMap String String -> (String, String)
generateStmtSr optionalPrefix index stmt typeMap floatMap = case stmt of

  -- assignment stmt of preinitialized variable
  AssignStmt lValue expr -> 
    let assignSrName = optionalPrefix ++ lValue ++ "_assignment_" ++ show index
    in (genAssignmentSr assignSrName index lValue expr typeMap floatMap) 
  
  -- Return stmt
  ReturnStmt expr -> 
    let returnSrName = optionalPrefix ++ "return_stmt_" ++ show index
    in genReturnStmtSr returnSrName index expr typeMap floatMap

    
  _ -> ("", "")-- error "Unsupported statement type" 




------------------------------------------------------------------------------------------------- Asssignment Subroutine generation

-- genAssignmentSr generates a subroutine call and definition for the assignment of an expression to a preinitialized
-- variable (within the .bss section). Another subroutine def is generated for the eval of the expression. It is called 
-- within the assignment subroutine. The output of the expr eval is placed into a different regeister depending on its 
-- type. Then it is moved directly into the memory location of the variable.
-- @param [name of subroutine], [index of stmt], [name of variable], [expression to be assigned]
genAssignmentSr :: String -> Integer -> String -> Expr -> TypeMap -> HashMap String String -> (String, String)
genAssignmentSr assignSrName index lValue expr typeMap floatMap = 
  let
    -- Generate expr eval subroutine
    exprEvalSrBaseName = assignSrName ++ "_" ++ lValue ++ "_expr_eval_" ++ show index
    (exprEvalSrDef, exprEvalSrName, _) = genExprEvalSr exprEvalSrBaseName 0 expr typeMap floatMap

    -- type-specific move command
    moveCommand = moveInstr_RegToVar (getExprType expr typeMap) lValue

    -- Generate subroutine call and def for assignment of expr to var
    assignSrCall = "\tcall " ++ assignSrName ++ "\n"
    assignmentSrDef = assignSrName ++ ":\n" ++
                      "\tcall " ++ exprEvalSrName ++ "\n" ++    -- call expr eval 
                      moveCommand ++ "\tret\n"                  -- move result into var 

    -- Combine the expr eval and assignment subroutine definitions
    fullSrDef = "\n;Assignment Statement\n" ++ assignmentSrDef ++ exprEvalSrDef

  in (assignSrCall, fullSrDef)


------------------------------------------------------------------------------------------------- Return Statements

-- genReturnStmtSr gemnerates a subroutine call and associated definitions for a return statement. 
-- At this stage, the compiler supports only the main function, so the return statement will exit
-- the program with a syscall to exit with the value being return set as the exit code. In the 
-- future, when user defined functions are added, the return statement must differentiate between
-- the program exit and a function return. NOTE that exit codes being returned must be in range of
-- 0-255.
genReturnStmtSr :: String -> Integer -> Expr -> TypeMap -> HashMap String String -> (String, String) 
genReturnStmtSr baseName index expr typeMap floatMap = 
  let
    -- gen subroutine for expression evaluation
    (exprEvalSrDef, exprEvalSrName, _) = genExprEvalSr baseName index expr typeMap floatMap

    -- gen subroutine for return statement
    returnSrName = baseName ++ "_return_" ++ show index
    returnSrCall = "\tcall " ++ returnSrName ++ "\n"

    returnSrDef = returnSrName ++ ":\n" ++
                  "\tcall " ++ exprEvalSrName ++ "\n" ++   -- eval expr
                  "\tmov rdi, rax\n" ++                    -- move result into rdi
                  "\tmov rax, 60\n" ++                     -- syscall for exit
                  "\tsyscall\n"                            -- exit

    -- Combine all subroutine defs
    fullSrDef = "\n;Return Statement\n" ++ returnSrDef ++ exprEvalSrDef

  in (returnSrCall, fullSrDef)