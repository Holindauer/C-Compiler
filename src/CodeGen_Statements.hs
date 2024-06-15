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
generateStmtSr :: String -> Integer -> Stmt -> TypeMap -> FloatMap -> (String, String)
generateStmtSr optionalPrefix index stmt typeMap floatMap = case stmt of

  -- assignment stmt of preinitialized variable
  AssignStmt lValue expr -> 
    let assignSrName = optionalPrefix ++ lValue ++ "_assignment_" ++ show index
    in (genAssignmentSr assignSrName index lValue expr typeMap floatMap) 
   
    -- declarations w/ assignment (specifically binary and unary ops that cannot be directly assigned in .data section)
  DeclarationAssignment dataType lValue expr -> case expr of
    BinOp _ _ _ -> getDeclarationAssignSr optionalPrefix index lValue expr typeMap floatMap
    UnaryOp _ _ -> getDeclarationAssignSr optionalPrefix index lValue expr typeMap floatMap
    _ -> ("", "") 

  -- simple declarations are ignored as they are handled in the .bss section
  SimpleDeclaration _ _ -> ("", "") 
  
  -- conditional stmt
  IfStmt condition thenBody elseBody -> 
    let conditionSrName = optionalPrefix ++ "cond_stmt_" ++ show index
    in (genConditionalSr conditionSrName index condition thenBody elseBody typeMap floatMap)

  -- else stmt
  ElseStmt body -> 
    let elseSrName = optionalPrefix ++ "else_stmt_" ++ show index
    in generateStmtSr elseSrName index (IfStmt (IntLit 1) body []) typeMap floatMap

  -- increment stmt
  IncrementStmt varName -> 
    let incrementSrName = optionalPrefix ++ "increment_stmt_" ++ show index
    in genIncrementSr incrementSrName index varName typeMap

  -- decrement stmt
  DecrementStmt varName -> 
    let decrementSrName = optionalPrefix ++ "decrement_stmt_" ++ show index
    in genDecrementSr decrementSrName index varName typeMap

  -- Return stmt
  ReturnStmt expr -> 
    let returnSrName = optionalPrefix ++ "return_stmt_" ++ show index
    in genReturnStmtSr returnSrName index expr typeMap floatMap

  _ -> ("", "")-- error "Unsupported statement type" 


-- genBodyOfStmts generates a subroutines for each stmt in a list of stmts, packages their calls 
-- together into a subroutine that will execute them in order, and returns the name of this subroutine
-- and all the aforementioned subroutine definitions concatenated together into a single string.
-- NOTE: the baseName is assumed to already be a unique name as part of a larger subroutine (if or loop)
genBodyOfStmts :: String -> [Stmt] -> TypeMap -> FloatMap -> (String, String)
genBodyOfStmts baseName stmts typeMap floatMap =
  let
    -- gen [(call, def)] for each stmt
    stmtsIndexed = zipWith (\i stmt -> (baseName ++ "_" ++ show i, i, stmt)) [0..] stmts       -- list of [(name, index, stmt)]
    stmtsSrTuples = map (\stmt -> uncurry3 generateStmtSr stmt typeMap floatMap) stmtsIndexed  -- pass (name, idx, stmt, typeMap) into generateStmtSr                        

    -- concat subroutine calls and definitions into single strings respectively
    stmtsSrDefs = concatMap (\(call, def) -> def) stmtsSrTuples                             
    stmtSrCalls = concatMap (\(call, def) -> call) stmtsSrTuples                           

    -- gen subroutine that will call each stmt in the body sequentially
    execBodySrName = baseName ++ "_execute_body"
    execBodySrDef = execBodySrName ++ ":\n" ++ stmtSrCalls ++ "\tret\n"   

    -- append the subroutine definitions together
    fullSrDef = execBodySrDef ++ stmtsSrDefs

  in (execBodySrName, fullSrDef)

  where 
    -- uncurry for 3 args
    uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
    uncurry3 f (x, y, z) = f x y z

------------------------------------------------------------------------------------------------- Asssignment Subroutine generation

-- genAssignmentSr generates a subroutine call and definition for the assignment of an expression to a preinitialized
-- variable (within the .bss section). Another subroutine def is generated for the eval of the expression. It is called 
-- within the assignment subroutine. The output of the expr eval is placed into a different regeister depending on its 
-- type. Then it is moved directly into the memory location of the variable.
-- @param [name of subroutine], [index of stmt], [name of variable], [expression to be assigned]
genAssignmentSr :: String -> Integer -> String -> Expr -> TypeMap -> FloatMap -> (String, String)
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


------------------------------------------------------------------------------------------------- Declaration Statements

-- getDeclarationAssignSr is a wrapper for genAssignmentSr that is used specifically for declarations with
-- assignment of binary and unary expressions. This function is used within a case statement in generateStmtSr
getDeclarationAssignSr :: String -> Integer -> String -> Expr -> TypeMap -> FloatMap -> (String, String)
getDeclarationAssignSr optionalPrefix index lValue expr typeMap floatMap = 
  let assignSrName = optionalPrefix ++ lValue ++ "_assignment_" ++ show index
  in (genAssignmentSr assignSrName index lValue expr typeMap floatMap)

-------------------------------------------------------------------------------------------------- Conditional Subroutine Generation

-- genConditionalSr generates a subroutine call and associated definitions for a conditional statement. This involves 
-- generating a subroutine for the evaluation of the conditional expression, a subroutine for the sequential execution 
-- of each statement within the then-body, and a subroutine that will call the conditional eval subroutine and, if the 
-- result is true, call the then body execution subroutine.
-- Generate subroutine for conditional statements including else branch handling
genConditionalSr :: String -> Integer -> Expr -> [Stmt] -> [Stmt] -> TypeMap -> FloatMap -> (String, String)
genConditionalSr baseName index condition thenBody elseBody typeMap floatMap =
  let
    -- Subroutine name for the conditional statement
    condSrName = baseName ++ "_" ++ show index

    -- Generate conditional evaluation subroutine
    evalCondSrBaseName = condSrName ++ "_eval_cond_" ++ show index
    (condEvalSrDef, condEvalSrName, _) = genExprEvalSr evalCondSrBaseName index condition typeMap floatMap

    -- Generate subroutine definitions and calls for each statement in the then-body
    (thenBodySrName, thenBodySrDef) = genBodyOfStmts (condSrName ++ "_then") thenBody typeMap floatMap

    -- Generate optional else body execution subroutine definition and name
    (execElseBodySrName, execElseBodySrDef) = genOptionalElseBody (condSrName ++ "_else") elseBody typeMap floatMap

    -- Generate subroutine definition and call for evaluating the condition and branching
    condSrCall = "\tcall " ++ condSrName ++ "\n"
    condSrDef = condSrName ++ ":\n" ++
                "\tcall " ++ condEvalSrName ++ "\n" ++
                "\tcmp rax, 1\n" ++   -- ! This contains an issue, integration tests will point it out
                "\tje " ++ thenBodySrName ++ "\n" ++
                (if not (null elseBody) then "\tjne " ++ execElseBodySrName ++ "\n" else "") ++
                "\tret\n"

    -- Combine all subroutine definitions together
    fullSrDef = "\n;Conditional Statement\n" ++ condSrDef ++ condEvalSrDef ++ thenBodySrDef ++
                (if not (null elseBody) then execElseBodySrDef else "")

  in (condSrCall, fullSrDef)

-- genOptionalElseBody generates the subroutine definition and call for each statement in the 
-- else body of a conditional statement. If there is no else body, it will return empty strings. 
genOptionalElseBody :: String -> [Stmt] -> TypeMap -> FloatMap -> (String, String)
genOptionalElseBody baseName elseBody typeMap floatMap
  | null elseBody = ("", "")                                       
  | otherwise     = genBodyOfStmts (baseName ++ "_else") elseBody  typeMap floatMap

-------------------------------------------------------------------------------------------------- Increment/Decrement Subroutine Generation

-- Generates subroutine definition and call for incrementing a variable
genIncrementSr :: String -> Integer -> String -> TypeMap -> (String, String)
genIncrementSr baseName index varName typeMap = 
  let
    -- unique subroutine names
    incrementSrName = baseName ++ show index

    -- gen subroutine cal and def 
    incrementSrCall = "\tcall " ++ incrementSrName ++ "\n" 
    incrementSrDef = incrementSrName ++ ":\n" ++
                     "\tinc [" ++ varName ++ "_label]\n" ++ "\tret\n" -- inc value in var and return                     

  in (incrementSrCall, incrementSrDef)


-- Generates subroutine defintion and call for decrementing a variable
genDecrementSr :: String -> Integer -> String -> TypeMap -> (String, String)
genDecrementSr baseName index varName typeMap = 
  let
    -- unique subroutine names
    decrementSrName = baseName ++ show index

    -- gen subroutine def
    decrementSrCall = "\tcall " ++ decrementSrName ++ "\n"
    decrementSrDef = decrementSrName ++ ":\n" ++
                     "\tdec [" ++ varName ++ "_label]\n" ++ "\tret\n"  -- dec val in var and return

  in (decrementSrCall, decrementSrDef)

------------------------------------------------------------------------------------------------- Return Statements

-- genReturnStmtSr gemnerates a subroutine call and associated definitions for a return statement. 
-- At this stage, the compiler supports only the main function, so the return statement will exit
-- the program with a syscall to exit with the value being return set as the exit code. In the 
-- future, when user defined functions are added, the return statement must differentiate between
-- the program exit and a function return. NOTE that exit codes being returned must be in range of
-- 0-255.
genReturnStmtSr :: String -> Integer -> Expr -> TypeMap -> FloatMap -> (String, String) 
genReturnStmtSr baseName index expr typeMap floatMap = 

  -- ! might be good to assert that the expr is of type int or char here
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