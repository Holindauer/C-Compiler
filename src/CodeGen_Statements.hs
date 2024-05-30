{-# LANGUAGE RankNTypes #-}

module CodeGen_Statements where

import System.IO
import AST
import Helper
import CodeGen_Expressions
import Data.List (foldl', zipWith)
import Debug.Trace (traceShow, trace)
import qualified Data.Map as Map

-- Issue:
-- Currently, the code generator generates subroutines using the same register, rax, for storage and return 
-- of all types of expression. This is fine for ints and char but an oversight regarding floating point type 
-- numbers which require different registers when handling. 

-- Gameplan:
-- I think the design implemented should be easily modified to accomodate for this. It will require that during
-- the generation of a subroutine chain for an expression eval, the type of the expr is passed along with the 
-- expr itself. Assuming correct syntax, pattern matching for the correct implementation of the handling of 
-- float/double vs int/char types should be straightforward. As long as the entire chain is consistent and the 
-- correct return register is known by the function calling the expr eval (in order to make the final assignment)
-- it should still work



-- CodeGen_Statements.hs contains functions for generating NASM assembly code for each type of statement. 
-- generateStmtSr is the master handler for Stmts collected by the parser, it delegates control to the 
-- apropriate subroutine generation function for the particular type of statement

-------------------------------------------------------------------------------------------------- Statement Subroutine Generation Master Function

-- generateStmtSr creates a subroutine call and its associated definitions for a single statement.
-- @param the statement and its index wrt to the list of statements collected by the pasrer
-- @dev There is also an optional baseName parameter that can be used to inject a unique prefix into the 
-- subroutine name. An empty string should be passed into the prefix if no prefix is desired.
generateStmtSr :: String -> Integer -> Stmt -> [(String, VarType)] -> (String, String)
generateStmtSr optionalPrefix index stmt typeList = case stmt of

  -- assignment stmt of preinitialized variable
  AssignStmt lValue expr -> 
    let assignSrName = optionalPrefix ++ lValue ++ "_assignment_" ++ show index
    in (genAssignmentSr assignSrName index lValue expr typeList) 

  -- declaration assignment stmt
  DeclarationAssignment dataType lValue expr -> 
    let assignSrName = optionalPrefix ++ lValue ++ "_assignment_" ++ show index
    in (genAssignmentSr assignSrName index lValue expr typeList)

  -- simple declarations are ignored as they are handled in the .bss section
  SimpleDeclaration _ _ -> ("", "")

  -- conditional stmt
  IfStmt condition thenBody elseBody -> 
    let conditionSrName = optionalPrefix ++ "cond_stmt_" ++ show index
    in (genConditionalSr conditionSrName index condition thenBody elseBody typeList)

  -- else stmt
  ElseStmt body -> 
    let elseSrName = optionalPrefix ++ "else_stmt_" ++ show index
    in generateStmtSr elseSrName index (IfStmt (IntLit 1) body []) typeList

  -- increment stmt
  IncrementStmt varName -> 
    let incrementSrName = optionalPrefix ++ "increment_" ++ show index
    in genIncrementSr incrementSrName index varName typeList

  -- decrement stmt
  DecrementStmt varName -> 
    let decrementSrName = optionalPrefix ++ "decrement_" ++ show index
    in genDecrementSr decrementSrName index varName typeList

  -- for loop stmt
  ForStmt initStmt condition updateStmt body -> 
    let forLoopSrName = optionalPrefix ++ "for_loop_" ++ show index
    in genForLoopSr forLoopSrName index initStmt condition updateStmt body typeList

  -- while loop stmt
  WhileStmt condition body -> 
    let whileLoopSrName = optionalPrefix ++ "while_loop_" ++ show index
    in genWhileLoopSr whileLoopSrName index condition body typeList

  -- Return stmt
  ReturnStmt expr -> 
    let returnSrName = optionalPrefix ++ "return_stmt_" ++ show index
    in genReturnStmtSr returnSrName index expr typeList

  _ -> error "Unsupported statement type" 


-- genBodyOfStmts generates a subroutines for each stmt in a list of stmts, packages their calls 
-- together into a subroutine that will execute them in order, and returns the name of this subroutine
-- and all the aforementioned subroutine definitions concatenated together into a single string.
-- NOTE: the baseName is assumed to already be a unique name as part of a larger subroutine (if or loop)
genBodyOfStmts :: String -> [Stmt] -> [(String, VarType)] -> (String, String)
genBodyOfStmts baseName stmts typeList = 
  let
    -- gen [(call, def)] for each stmt
    stmtsIndexed = zipWith (\i stmt -> (baseName ++ "_" ++ show i, i, stmt)) [0..] stmts -- list of [(name, index, stmt)]
    stmtsSrTuples = map (\stmt -> uncurry3 generateStmtSr stmt typeList) stmtsIndexed    -- pass (name, idx, stmt, typeList) into generateStmtSr                        

    -- concat subroutine calls and definitions into single strings respectively
    stmtsSrDefs = concatMap (\(call, def) -> def) stmtsSrTuples                             
    stmtSrCalls = concatMap (\(call, def) -> call) stmtsSrTuples                           

    -- gen subroutine that will call each stmt in the body sequentially
    execBodySrName = baseName ++ "_execute_body"
    execBodySrDef = execBodySrName ++ ":\n" ++ stmtSrCalls ++ "\tret\n"   

    -- append the subroutine definitions together
    fullSrDef = execBodySrDef ++ stmtsSrDefs

  in (execBodySrName, fullSrDef)

------------------------------------------------------------------------------------------------- Asssignment Subroutine generation

-- genAssignmentSr generates a subroutine call and definition for the assignment of an expression to a preinitialized
-- variable (within the .bss section). Another subroutine def is generated for the eval of the expression. It is called 
-- within the assignment subroutine. The output of the expr eval is placed into a different regeister depending on its 
-- type. Then it is moved directly into the memory location of the variable.
-- @param [name of subroutine], [index of stmt], [name of variable], [expression to be assigned]
genAssignmentSr :: String -> Integer -> String -> Expr -> [(String, VarType)] -> (String, String)
genAssignmentSr assignSrName index lValue expr typeList = 
  let
    -- Generate expression evaluation subroutine
    exprEvalSrBaseName = assignSrName ++ "_" ++ lValue ++ "_expr_eval_" ++ show index
    (exprEvalSrDef, exprEvalSrName, _) = genExprEvalSr exprEvalSrBaseName 0 expr 

    -- Type specific move command
    moveCommand = moveOutputIntoVarInstr (getExprType expr) lValue

    -- Generate subroutine call and def for assignment of expr to var
    assignSrCall = "\tcall " ++ assignSrName ++ "\n"
    assignmentSrDef = assignSrName ++ ":\n" ++
                      "\tcall " ++ exprEvalSrName ++ "\n" ++      -- Call expr eval subroutine, result in rax
                      moveCommand ++ "\tret\n"                    -- Move result into var mem and return 

    -- Combine the expr eval and assignment subroutine definitions
    fullSrDef = assignmentSrDef ++ exprEvalSrDef

  in (assignSrCall, fullSrDef)

-------------------------------------------------------------------------------------------------- Conditional Subroutine Generation


-- genConditionalSr generates a subroutine call and associated definitions for a conditional statement. This involves 
-- generating a subroutine for the evaluation of the conditional expression, a subroutine for the sequential execution 
-- of each statement within the then-body, and a subroutine that will call the conditional eval subroutine and, if the 
-- result is true, call the then body execution subroutine.
genConditionalSr :: String -> Integer -> Expr -> [Stmt] -> [Stmt] -> [(String, VarType)] -> (String, String)
genConditionalSr baseName index condition thenBody elseBody typeList = 
  let 
    -- subroutine name for the conditional statement 
    condSrName = baseName ++ "_" ++ show index                                            
    
    -- gen conditional eval subroutine   
    evalCondSrBaseName = condSrName ++ "_eval_cond_" ++ show index      
    (condEvalSrDef, condEvalSrName, _) = genExprEvalSr evalCondSrBaseName index condition 

    -- gen subroutine defs and calls for each statement in the then-body 
    (thenBodySrName, thenBodySrDef) = genBodyOfStmts (condSrName ++ "_then") thenBody typeList
    
    -- gen optional else body execution sr def and name (will be empty if no else body)
    (execElseBodySrName, execElseBodySrDef) = genOptionalElseBody baseName elseBody typeList

    -- gen sr def and call for calling the condition eval sr, executing then body if
    -- result in rax is true, or calling the else body if one exists and the result is false 
    condSrCall = "\tcall " ++ condSrName ++ "\n"          -- call
    condSrDef = condSrName ++ ":\n" ++                    -- def
                "\tcall " ++ condEvalSrName ++ "\n" ++    -- eval condition (result in rax)
                "\tcmp rax, 1\n" ++                       -- compare val in rax to 1
                "\tje " ++ thenBodySrName ++ "\n" ++      -- jump to then-body execution if true
                (if not (null elseBody) then "\tjne " ++ execElseBodySrName ++ "\n" else "") ++ -- jump to else body if it exists and cond is false
                "\tret\n"                               

    -- Combine all subroutine definitions together
    fullSrDef = condSrDef ++ condEvalSrDef ++ thenBodySrDef ++ execElseBodySrDef 

  in (condSrCall, fullSrDef)

-- genOptionalElseBody generates the subroutine definition and call for each statement in the 
-- else body of a conditional statement. If there is no else body, it will return empty strings. 
genOptionalElseBody :: String -> [Stmt] -> [(String, VarType)] -> (String, String)
genOptionalElseBody baseName elseBody typeList
  | null elseBody = ("", "")                                       
  | otherwise     = genBodyOfStmts (baseName ++ "_else") elseBody  typeList

-------------------------------------------------------------------------------------------------- For Loop Subroutine Generation

-- genForLoopSr generates a call and associated definitions for a for loop. This involves generating a subroutine
-- for the initialization statement, a subroutine for the update statement, a subroutine for the evaluation of the
-- loop termination condition, a subroutine for the sequential execution of each statement within the body of the loop,
-- and a subroutine that will call the initialization statement, the condition eval subroutine, and if the result is true,
-- call the update statement and the body execution subroutine.
genForLoopSr :: String -> Integer -> Stmt -> Expr -> Stmt -> [Stmt] -> [(String, VarType)] -> (String, String)
genForLoopSr baseName index initStmt condition updateStmt body typeList =
  let
    -- gen subroutine for loop counter init
    initStmtSrName = baseName ++ "_init_stmt_" ++ show index
    (initStmtSrCall, initStmtSrDef) = generateStmtSr initStmtSrName index initStmt typeList

    -- gen subroutine for update statement
    updateStmtSrName = baseName ++ "_update_stmt_" ++ show index
    (updateStmtSrCall, updateStmtSrDef) = generateStmtSr updateStmtSrName index updateStmt typeList

    -- gen subroutine for loop termination condition eval
    (conditionSrDef, conditionSrName, _) = genExprEvalSr baseName index condition

    -- gen subroutine for each stmt in the loop body 
    (exectuteBodySrName, bodySrDef) = genBodyOfStmts (baseName ++ "_body") body typeList

    -- gen subroutine to call the loop counter init, eval loop termination condition and if true call update statement and loop body, 
    forLoopSrName = baseName ++ "_looper_" ++ show index
    forLoopSrCall = "\tcall " ++ forLoopSrName ++ "\n"                     
    forLoopSrDef = forLoopSrName ++ ":\n" ++                             -- kickstart loop def            
                  initStmtSrCall ++                                      -- call init stmt
                  "\tjmp for_loop_condition_" ++ show index ++ "\n" ++   -- jump to condition check

                  -- condition check sr def
                  "for_loop_condition_" ++ show index ++ ":\n" ++        
                  "\tcall " ++ conditionSrName ++ "\n" ++                -- call condition eval
                  "\tcmp rax, 0\n" ++                                    -- compare result to 0
                  "\tje for_loop_end_" ++ show index ++ "\n" ++          -- jump to end of loop if false
                  "\tcall " ++ exectuteBodySrName ++ "\n" ++             -- exec loop body 
                  updateStmtSrCall ++                                    -- call update stmt 
                  "\tjmp for_loop_condition_" ++ show index ++ "\n" ++   -- jump back to condition check

                  -- end loop sr def
                  "for_loop_end_" ++ show index ++ ":\n" ++ "\tret\n"

    -- Combine all subroutine definitions together
    fullSrDef = forLoopSrDef ++ initStmtSrDef ++ conditionSrDef ++ updateStmtSrDef ++ bodySrDef

  in (forLoopSrCall, fullSrDef)

-------------------------------------------------------------------------------------------------- While Loop Subroutine Generation

-- genWhileLoopSr generates the NASM assembly code for a while loop. This involves generating a subroutine
-- for the evaluation of the conditional expression, a subroutine for the sequential execution of each statement within
-- the body of the while loop, and a subroutine that will call the conditional eval subroutine and, if the result is true,
-- call the body execution subroutine.
genWhileLoopSr :: String -> Integer -> Expr -> [Stmt] -> [(String, VarType)] -> (String, String) 
genWhileLoopSr baseName index condition body typeList =
  let
    
    -- gen subroutine for termination condition expr
    (conditionSrDef, conditionSrName, _) = genExprEvalSr baseName 0 condition

    -- gen subroutine for the loop body 
    (bodySrBaseName, bodySrDef) = genBodyOfStmts (baseName ++ "_body") body typeList

    -- generate subroutine for while loop  
    whileLoopSrName = baseName ++ "_looper_" ++ show index
    whileLoopSrCall = "\tcall " ++ whileLoopSrName ++ "\n" -- call
    whileLoopSrDef = whileLoopSrName ++ ":\n" ++           -- kickstart loop def                
                  "\tjmp while_loop_condition_" ++ show index ++ "\n" ++   -- jump to condition check

                  -- eval condition sr def
                  "while_loop_condition_" ++ show index ++ ":\n" ++       
                  "\tcall " ++ conditionSrName ++ "\n" ++                 -- condition eval 
                  "\tcmp rax, 0\n" ++                                     -- compare result to 0
                  "\tje while_loop_end_" ++ show index ++ "\n" ++         -- jump to loop end if false (zero)
                  "\tcall " ++ bodySrBaseName ++ "\n" ++                  -- call loop body exec
                  "\tjmp while_loop_condition_" ++ show index ++ "\n" ++  -- jump back to condition check

                  -- loop end def
                  "while_loop_end_" ++ show index ++ ":\n" ++ "\tret\n"

    -- Combine all subroutine defs 
    fullSrDef = whileLoopSrDef ++ conditionSrDef ++ bodySrDef 

  in (whileLoopSrCall, fullSrDef)

-------------------------------------------------------------------------------------------------- Increment/Decrement Subroutine Generation

-- Generates subroutine definition and call for incrementing a variable
genIncrementSr :: String -> Integer -> String -> [(String, VarType)] -> (String, String)
genIncrementSr baseName index varName typeList = 
  let
    -- unique subroutine names
    incrementSrName = baseName ++ show index

    -- gen subroutine cal and def 
    incrementSrCall = "\tcall " ++ incrementSrName ++ "\n" 
    incrementSrDef = incrementSrName ++ ":\n" ++
                     "\tinc [" ++ varName ++ "_label]\n" ++ "\tret\n" -- inc value in var and return                     

  in (incrementSrCall, incrementSrDef)


-- Generates subroutine defintion and call for decrementing a variable
genDecrementSr :: String -> Integer -> String -> [(String, VarType)] -> (String, String)
genDecrementSr baseName index varName typeList = 
  let
    -- unique subroutine names
    decrementSrName = baseName ++ show index

    -- gen subroutine def
    decrementSrCall = "\tcall " ++ decrementSrName ++ "\n"
    decrementSrDef = decrementSrName ++ ":\n" ++
                     "\tdec [" ++ varName ++ "_label]\n" ++ "\tret\n"  -- dec val in var and return

  in (decrementSrCall, decrementSrDef)

-------------------------------------------------------------------------------------------------- Return Stmt Subroutine Generation

-- genReturnStmtSr gemnerates a subroutine call and associated definitions for a return statement. 
-- At this stage, the compiler supports only the main function, so the return statement will exit
-- the program with a syscall to exit with the value being return set as the exit code. In the 
-- future, when user defined functions are added, the return statement must differentiate between
-- the program exit and a function return.
genReturnStmtSr :: String -> Integer -> Expr -> [(String, VarType)] -> (String, String)
genReturnStmtSr baseName index expr typeList = 
  let
    -- gen subroutine for expression evaluation
    (exprEvalSrDef, exprEvalSrName, _) = genExprEvalSr baseName index expr

    -- gen subroutine for return statement
    returnSrName = baseName ++ "_return_" ++ show index
    returnSrCall = "\tcall " ++ returnSrName ++ "\n"

    returnSrDef = returnSrName ++ ":\n" ++
                  "\tcall " ++ exprEvalSrName ++ "\n" ++   -- eval expr
                  "\tmov rdi, rax\n" ++                    -- move result into rdi
                  "\tmov rax, 60\n" ++                     -- syscall for exit
                  "\tsyscall\n"                            -- exit

    -- Combine all subroutine defs
    fullSrDef = returnSrDef ++ exprEvalSrDef

  in (returnSrCall, fullSrDef)


