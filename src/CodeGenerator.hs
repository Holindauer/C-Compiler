{-# LANGUAGE RankNTypes #-}


module CodeGenerator where

import System.IO
import AST
import Data.List (foldl', zipWith)
import qualified Data.Map as Map

-- Code Genetor Design and Implementation Strategy:
-- The Code generator is used to transalate the list of statements returned by the Parser into NASM x86-64 assembly 
-- code. 

-- The general design of the code generator is to first determine how much stack space is needed for all declared 
-- variables within the program and create a .bss section within the assembly file that allocates such space to 
-- variable names that match those in the source file. Then to transalte each statement in that list into a series 
-- of NASM assembly subroutines that when called in sequence will execute the program.
--
-- The calls to each subroutine of instructions for each statement in the program will be collected into the _start 
-- entry point for the program.  
--
-- There are four main types of subroutine for each statement in the program: 
--
--   1. Assignment 
--   2. Expression Evaluation
--   3. Conditional
--   4. For Loop
--   5. While Loop
--
-- It should be noted that only Assignment subroutines will change the state of the program. The other subroutines will 
-- determine control flow, intermediate evaluations, and repetition of other subroutines.
--
-- The process for converting Assignment statement into the text of a subroutine involves two steps. The first is to evaluate
-- the expression on the right hand side of the statement into an intermediate value register. The second step is to store 
-- the value that was placed into the intermediate register into the appropritate variable in the .bss section
--
-- The process for evaluating an expression is potentially complex. In its simplest cases, the expression is a literal or the
-- value of a variable. If the expression involves unary or binary operations, the evaluation will involve recursively chaining
-- subroutines together based on the grammatical structure of the AST in a depth first manner. Calling the head subroutine of
-- the chain will form a sort ofpseudo-recursive chain of subroutine calls that will execute the evaluation of the expression, 
-- placing it into an intermediate output register to be used for the ultimate assignment into the correct variable in the .bss 
-- section.
--
-- The process for translating an expression statement into a subroutine is outlined as follows:
--
-- In either of these cases, if an expression is to be evaluated, a subroutine definition will be created with a unique name 
-- that will be called within the subroutine where the expression was encounted. Literals/Variables are the base case of this 
-- process, Unary/Binary are recursive cases. Each subroutine will evaluate the expression to which its operation is applied 
-- and move its value into an intermediate output register. Literal/Variable expression will only do this. If the expression is unary
-- or Binary, a call to another subroutine will be made to handle it, with its associated definition being created at this point as 
-- well. When that subroutine returns, the value of the expresssion will have be placed into an intermdiate outptu register that the 
-- calling subroutine will use as an argument for its own operation. The only caveat to this concerns binary operations, because 
-- their evaluation requires the evaluation of a left and right hand side expression, implicating the need for a different strategy
-- for saving these two values. To resolve this, the evaluation of the left hand side will be placed into dynamically allocated 
-- space that will be freed once binary operation has been applied to both values.
--
-- After the depth first evaluated is complete, the value will be moved into the variable declared in the .bss section that corresponds
-- to the assignment statement in question.
-- 
-- Once the assignments statement and expression evaluation template subroutine generator functions are implemented, conditional statements
-- will be implementable by writing a subroutine that will recursively implement the expression evaluation of the condition, then jumping 
-- a subroutine that contains subroutines for each statement in the body of the conditional. This will be done using the same depth wise
-- evaluation process but applied to sequential statements in the body of the conditional.
--
-- Loops will be a matter of implementing a conditional statement in the way described above where the body of the loop is called until
-- the end condition is met.


-- Master code generation function
generateCode :: Program -> String
generateCode program =
  let
    bssSection = generateBssSection program
    textTuples = generateTextSection program

    -- placeholder for actual assembly of text section from textTuples
    textSection = concatMap (\(call, def) -> call ++ def) textTuples
  in
    "section .bss\n" ++ bssSection ++ "section .text\n" ++ textSection


-- Writes the assembly code to a file
writeToFile :: FilePath -> String -> IO ()
writeToFile path content = writeFile path content

-------------------------------------------------------------------------------------------------- .bss section generation

-- generateBssSection perfroms a left fold over the list of statements returned by the parser
-- It locates all variable declarations at all levels of indentation within the program, and 
-- generates the corresponding BSS section in NASM assembly syntax. 
-- @dev the BSS section is where uninitialized data is allocated in memory at compile time 
generateBssSection :: [Stmt] -> String
generateBssSection stmts = foldl' generateBssText "" stmts -- left fold over the list of stmts into an empty string
  where
    -- generateBssText creates a bss variable declaration in NASM assembly syntax for a single var
    generateBssText :: String -> Stmt -> String
    generateBssText bssAccumulator stmt = case stmt of

      -- NOTE: dataType, varName are Strings

      -- Simple variable declaration
      SimpleDeclaration dataType (Var varName) ->  
        appendBss bssAccumulator dataType varName
      
      -- Variable declaration with assignment (assignment handled in .text section)
      DeclarationAssignment dataType varName _ ->
        appendBss bssAccumulator dataType varName
      
      -- For loop incrementer declaration
      ForStmt initStmt _ _ body ->

        -- accumulate the incrementer variable declaration in the BSS section and append 
        -- output of recursive call to generateBssSection for the body of the for loop
        processComplexStmt bssAccumulator [initStmt] ++ generateBssSection body

      -- While Loop body
      WhileStmt _ body ->   
        -- recursive call on the body of the while loop
        generateBssSection body ++ bssAccumulator 

      -- If-Else statement body
      IfStmt _ thenBody elseBody ->
        let
            thenBss = generateBssSection thenBody -- recursive call on the then body
            elseBss = generateBssSection elseBody -- recursive call on the else body
        in bssAccumulator ++ thenBss ++ elseBss

      _ -> bssAccumulator  -- Ignore other statements not involved in variable declaration

    -- appendBss creates a variable declaration in NASM assembly syntax for a single variable
    -- It appends the variable declaration to the BSS section accumulator
    appendBss :: String -> String -> String -> String
    appendBss bssAccumulator dataType varName =
      let
        label = "\t" ++ varName ++ "_label"
        size = dataTypeToSize dataType
      in bssAccumulator ++ label ++ ": " ++ size ++ "\n"

    -- processComplexStmt recursively processes the body of a complex statement
    processComplexStmt :: String -> [Stmt] -> String
    processComplexStmt bssAccumulator stmts = foldl' generateBssText bssAccumulator stmts

-- Helper function for generating the size of a data type in NASM assembly syntax
dataTypeToSize :: String -> String
dataTypeToSize dataType = case dataType of
  "int" -> "resd 1"    -- Reserve space for 1 integer (4 bytess)
  "float" -> "resd 1"  -- 1 float (4 bytes)
  "double" -> "resq 1" -- 1 double (8 bytes)
  "char" -> "resb 1"   -- 1 char (1 byte)
  _ -> error "Unsupported data type"


-------------------------------------------------------------------------------------------------- .text section generation

-- generateTextSection returns a list of tuples containing subroutine calls and their definitions
generateTextSection :: [Stmt] -> [(String, String)]
generateTextSection stmts = map (uncurry generateStmtSr) indexedStmts -- uncurry unpacks tuple
  where
    -- zipWith is used to pair each statement with its index to create unique subroutine names
    indexedStmts = zipWith (,) [0..] stmts
    
-- generateStmtSubroutine creates a subroutine call and its associated definition for a single statement.
-- @param the statement number and the statement itself
generateStmtSr :: Integer -> Stmt -> (String, String)
generateStmtSr index stmt = case stmt of

  -- assignment of more complex expressions
  AssignStmt lValue expr -> genExprAssignmentSr index lValue expr
  _ -> error "Unsupported statement type"

-------------------------------------------------------------------------------------------------- Asssignment Subroutine generation

-- genExprAssignment generates the NASM assembly code for an assignment of a complex expression
-- to a variable that has been preinitialized within the .bss section. The subroutine will contain instructions
-- for evaluating the expression and moving the result into the variable's memory.
genExprAssignmentSr :: Integer -> String -> Expr -> (String, String)
genExprAssignmentSr index lValue expr = 
  let
    -- Set unique subroutine names
    assignSrName = lValue ++ "_assignment_" ++ show index
    exprEvalSrName = lValue ++ "_expr_eval_" ++ show index

    -- Call to generate the expression evaluation subroutine
    (exprEvalCode, _) = genExprEvalSr exprEvalSrName 0 expr

    -- Set the subroutine definition for the assignment that calls to the expression evaluation subroutine
    assignSrCall = "\tcall " ++ assignSrName ++ "\n"

    assignmentSrDef = assignSrName ++ ":\n" ++
                      "\tcall " ++ exprEvalSrName ++ "_0" ++ "\n" ++     -- ++ "0" bc we want to call the head of the subroutine chain
                      "\tmov rax, [rbp + " ++ lValue ++ "_label]\n" ++  -- Move the result of the expression eval into rax
                      "\tmov [" ++ lValue ++ "_label], rax\n" ++        -- Move rax into lValue memory
                      "\tret\n"                                         -- Return from subroutine

    -- Combine the expression eval subroutine definition with the assignment subroutine definition
    fullSrDef = assignmentSrDef ++ exprEvalCode

  in (assignSrCall, fullSrDef)

-------------------------------------------------------------------------------------------------- Expression Evaluation Subroutine generation

-- genExprSr recursively chains subroutine definitions together for evaluating expressions in a depth first manner. 
-- Literal/Variable expressions are the base case. Unary/Binary expressions will result in recursively chaining of
-- subroutines to handle the evaluation of the subexpressions.
-- @param The base name of expression evaluation subroutine, the number of subroutines that have already been chained 
-- from the base to get to this one, and the current expr/subexpr.
genExprEvalSr :: String -> Integer -> Expr -> (String, Integer)
genExprEvalSr baseName index expr = case expr of

  -- Base Cases: expr is a literal or variable
  IntLit value ->
    literalEvalSr baseName index value "Int"
  FloatLit value ->
    literalEvalSr baseName index value "Float"
  DoubleLit value ->
    literalEvalSr baseName index value "Double"
  CharLit value ->
    literalEvalSr baseName index value "Char"
  Var varName -> 
    variableEvalSr baseName index varName

  -- Recursive Cases: expr is a unary or binary operation 
  UnaryOp op subExpr ->
    unaryOpEvalSr baseName index op subExpr
  BinOp op lhs rhs ->
    binaryOpEvalSr baseName index op lhs rhs
    
  _ -> error "Unsupported expression type"


-- Helper func to generate subroutine that moves a literal value into rax during expressions eval 
literalEvalSr :: String -> Integer -> Show a => a -> String -> (String, Integer)
literalEvalSr baseName index value typeName =
  let
    -- set unique subroutine name
    subroutineName = baseName ++ "_lit_" ++ typeName ++ "_" ++ show value ++ "_" ++ show index

    -- set subroutine definition
    subroutineDef = subroutineName ++ ":\n" ++
                    "\tmov rax, " ++ show value ++ "\n" ++
                    "\tret\n"
  in (subroutineDef, index) -- return subroutine def updated index


-- Helper func to generate subroutnie that moves the value of a variable into rax during expression eval
variableEvalSr :: String -> Integer -> String -> (String, Integer)
variableEvalSr baseName index varName =
  let
      -- set unique subroutine name derived from base name, varName, and index
      subroutineName = baseName ++ "_var_" ++ varName ++ "_" ++ show index

      -- set subroutine definition 
      subroutineDef = subroutineName ++ ":\n" ++                   -- subroutine label
                      "\tmov rax, [" ++ varName ++ "_label]\n" ++  -- move value of varName into rax
                      "\tret\n"                                    -- return from subroutine

    -- return the subroutine definition and the updated index
    in (subroutineDef, index)

-- Helper func to generate subroutine that evaluates a unary operation recursively by chaining
-- subroutine definitions for evaluating all subexpressions together. The output of the subexpression 
-- is stored in the rax register the unary operation is applied.
unaryOpEvalSr :: String -> Integer -> UnaryOp -> Expr -> (String, Integer)
unaryOpEvalSr baseName index op subExpr =
  let
      -- recursively evaluate the subexpression. NOTE that the index passed in is incremented
      (subExprDef, newIndex) = genExprEvalSr baseName (index + 1) subExpr

      -- set unique subroutine name derived from base name, unary op, and index
      subroutineName = baseName ++  "_" ++ show index

      -- set subroutine definition 
      subroutineDef = subroutineName ++ ":\n" ++  -- subroutine label
                      unaryOpAsm op ++            -- use unaryOpAsm to generate instructions for specific op
                      "\tret\n"                   -- return from subroutine

      -- append the subExprDef to the subroutine definition
      fullSubroutineDef = subroutineDef ++ subExprDef

    -- return the full subroutine definition and the updated index
    in (fullSubroutineDef, newIndex)

-- Helper func to generate a subroutine that evaluates a binary operation recursively by 
-- generating a chain of subroutine definitions that will evaluate the left and right hand
-- subexpressions in a depth first manner. The output of the left hand side expression is
-- stored on the stack before evaluating the rhs. The final result is stored in the rax register.
binaryOpEvalSr :: String -> Integer -> Op -> Expr -> Expr -> (String, Integer)
binaryOpEvalSr baseName index op lhs rhs =
  let
      -- Generate the subroutine names for LHS and RHS evaluations
      lhsExprEvalSrName = baseName ++ "_lhs_eval_" ++ show (index + 1)
      rhsExprEvalSrName = baseName ++ "_rhs_eval_" ++ show (index + 2)

      -- Generate subroutine definitions for evaluating LHS and RHS
      (lhsExprEvalSrDef, lhsLastIndex) = genExprEvalSr baseName (index + 1) lhs
      (rhsExprEvalSrDef, rhsLastIndex) = genExprEvalSr baseName (lhsLastIndex + 1) rhs

      -- Unique subroutine name for the binary operation
      binaryOpSrName = baseName ++ "_" ++ show index

      -- Define the binary operation subroutine
      binaryOpSrDef = binaryOpSrName ++ ":\n" ++

          -- Evaluate the LHS expression and store the result temporarily
          "\tcall " ++ baseName ++ "\n" ++
          "\tpush rax\n" ++  -- Store LHS result on the stack

          -- Evaluate the RHS expression
          "\tcall " ++ rhsExprEvalSrName ++ "\n" ++
          "\tmov rbx, rax\n" ++  -- Move RHS result to rbx
          "\tpop rax\n" ++  -- Retrieve LHS result from stack to rax

          -- generate the binary operation instructions using binaryOpAsm
          binaryOpAsm op ++ 
          "\tret\n"

      -- Combine the subroutine definitions with the binary operation code
      fullBinaryOpSrDef = lhsExprEvalSrDef ++ rhsExprEvalSrDef ++ binaryOpSrDef

    -- Return the complete binary operation code and the last index used
    in (fullBinaryOpSrDef, rhsLastIndex)


-- unaryOpAsm is a helper function called within genExprEvalSr that generates the 
-- NASM assembly code for a specific unary operation based on the provided unary op.
-- It is assumed that the value for which the operation is being applied is already
-- within the rax register.
unaryOpAsm :: UnaryOp -> String
unaryOpAsm op = case op of
  Neg -> "\tneg rax\n"
  LogicalNot -> "\tnot rax\n"
  Increment -> "\tinc rax\n"
  Decrement -> "\tdec rax\n"
  _ -> error "Operation not supported"

-- binaryOpAsm is a helper function called within genExprEvalSr that generates the
-- NASM assembly code for a specific binary operation based on the provided binary op.s
-- It is assumed that the values for which the operation is being applied are already
-- within the rax and rbx registers. Commutativity does not apply to this setup.
binaryOpAsm :: Op -> String
binaryOpAsm op = case op of
  Add -> "\tadd rax, rbx\n"
  Subtract -> "\tsub rax, rbx\n"
  Multiply -> "\timul rax, rbx\n"
  Divide -> "\tidiv rbx\n"  -- Assume rbx holds the divisor
  _ -> error "Operation not supported"
