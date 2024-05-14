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


-- Master code generation function (currently placeholder)
generateCode :: Program -> String
generateCode program =
  let
    bssSection = generateBssSection program
    textTuples = generateTextSection program

    -- Concatenate the subroutine definitions into a single string
    subroutineDefs = concatMap (\(call, def) -> def) textTuples

    -- Concatenate the subroutine calls into a single string
    subroutineCalls = concatMap (\(call, def) -> call) textTuples

    -- The _start entry point for the program
    textSection = 
      -- start/main of the program
      "global _start\n" ++ 
      "section .text\n\n" ++
      "_start:\n" ++ 
      
      -- sequential subroutine calls
      "\t; Call the main subroutine\n" ++
      subroutineCalls ++

      -- exit program
      "\n\t; Exit the program properly\n" ++
	    "\tmov rax, 60      ; syscall number for exit\n" ++
	    "\txor rdi, rdi     ; exit status 0\n" ++
      "\tsyscall          ; perform the system call to exit\n\n" ++
      
      -- Subroutine definitions
      subroutineDefs

  in
    "section .bss\n" ++ bssSection ++ "\n" ++ textSection


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
  "int" -> "resq 1"    -- 1 int (8 bytes)
  "float" -> "resd 1"  -- 1 float (4 bytes)
  "double" -> "resq 1" -- 1 double (8 bytes)
  "char" -> "resb 1"   -- 1 char (1 byte)
  _ -> error "Unsupported data type"


-------------------------------------------------------------------------------------------------- .text section generation

-- generateTextSection returns a list of tuples containing subroutine calls and their definitions
generateTextSection :: [Stmt] -> [(String, String)]
generateTextSection stmts = map (uncurry3 generateStmtSr) indexedStmts
  where
    -- Creates list of empty strings the same length as stmts
    emptyStrings = replicate (length stmts) ""
  
    -- zip3 pairs each stmt with its index and an empty string prefix to create unique subroutine names
    indexedStmts = zip3 emptyStrings [0..] stmts 

-- uncurry3 is a modified version of the uncurry built-in function that works with 3-tuples
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z


-- generateStmtSubroutine creates a subroutine call and its associated definition for a single statement.
-- @param the statement number/index and the statement itself
-- @dev There is also an optional baseName parameter that can be used to inject a unique prefix into the 
-- subroutine name. An empty string should be passed into the prefix if no prefix is desired.
generateStmtSr :: String -> Integer -> Stmt -> (String, String)
generateStmtSr optionalPrefix index stmt = case stmt of

  -- assignment stmt
  AssignStmt lValue expr -> 
    let assignSrName = optionalPrefix ++ lValue ++ "_assignment_" ++ show index
    in (genAssignmentSr assignSrName index lValue expr) 

  -- declaration assignment stmt
  DeclarationAssignment dataType lValue expr -> 
    let assignSrName = optionalPrefix ++ lValue ++ "_assignment_" ++ show index
    in (genAssignmentSr assignSrName index lValue expr)

  -- simple declarations are ignored as they are handled in the .bss section
  SimpleDeclaration _ _ -> ("", "")

  -- conditional stmt
  IfStmt condition thenBody elseBody -> 
    let conditionSrName = optionalPrefix ++ "cond_stmt_" ++ show index
    in (genConditionalSr conditionSrName index condition thenBody elseBody)

  -- increment stmt
  IncrementStmt varName -> 
    let incrementSrName = optionalPrefix ++ "increment_" ++ show index
    in genIncrementSr incrementSrName index varName

  -- decrement stmt
  DecrementStmt varName -> 
    let decrementSrName = optionalPrefix ++ "decrement_" ++ show index
    in genDecrementSr decrementSrName index varName

  -- for loop stmt
  ForStmt initStmt condition updateStmt body -> 
    let forLoopSrName = optionalPrefix ++ "for_loop_" ++ show index
    in genForLoopSr forLoopSrName index initStmt condition updateStmt body

  _ -> error "Unsupported statement type"

-------------------------------------------------------------------------------------------------- Asssignment Subroutine generation

-- genExprAssignment generates the NASM assembly code for an assignment of a complex expression
-- to a variable that has been preinitialized within the .bss section. The subroutine will contain instructions
-- for evaluating the expression and moving the result into the variable's memory.
genAssignmentSr :: String -> Integer -> String -> Expr -> (String, String)
genAssignmentSr assignSrName index lValue expr = 
  let
    -- Set unique subroutine names
    exprEvalSrName = lValue ++ "_expr_eval_" ++ show index

    -- Call to generate the expression evaluation subroutine
    (exprEvalSrDef, _, _) = genExprEvalSr exprEvalSrName 0 expr

    -- Set the subroutine definition for the assignment that calls to the expression evaluation subroutine
    assignSrCall = "\tcall " ++ assignSrName ++ "\n"

    assignmentSrDef = assignSrName ++ ":\n" ++
                      "\tcall " ++ exprEvalSrName ++ "_0" ++ "\n" ++     -- ++ "0" bc we want to call the head of the subroutine chain
                      "\tmov [" ++ lValue ++ "_label], rax\n" ++        -- Move the result in rax directly into the variable's memory
                      "\tret\n"                                         -- Return from subroutine

    -- Combine the expression eval subroutine definition with the assignment subroutine definition
    fullSrDef = assignmentSrDef ++ exprEvalSrDef

  in (assignSrCall, fullSrDef)

-------------------------------------------------------------------------------------------------- Conditional Subroutine Generation


-- genConditionalSr generates the NASM assembly code for a conditional statement. This involves generating a subroutine
-- for the evaluation of the conditional expression, a subroutine for the sequential execution of each statement within
-- the then Body, and a subroutine that will call the conditional eval subroutine and, if the result is true, call the
-- then body execution subroutine.
genConditionalSr :: String -> Integer -> Expr -> [Stmt] -> [Stmt] -> (String, String)
genConditionalSr baseName index condition thenBody elseBody = 
  let 
    
    -- Step 1: make subroutine that evaluates the conditional expression
    condSrName = baseName ++ "_" ++ show index                                            -- name of subroutine chain head
    evalCondSrBaseName = condSrName ++ "_eval_cond_" ++ show index                        -- base name for conditional eval subroutine
    (condEvalSrDef, condEvalSrName, _) = genExprEvalSr evalCondSrBaseName index condition -- generate the conditional eval subroutine

    -- Step 2: make subroutines for each statement in the then body of the conditional
    thenBodySrBaseName = condSrName ++ "_then_body_" 
    thenBodyIndexed = zipWith (\i stmt -> (thenBodySrBaseName ++ show i, i, stmt)) [0..] thenBody  -- makes [(name, index, stmt)]
    thenBodySrTuples = map (uncurry3 generateStmtSr) thenBodyIndexed                               -- generates [(call, def)] for each stmt
    thenBodySrDef = concatMap (\(call, def) -> def) thenBodySrTuples                               -- concatenates all subroutine definitions

    -- Step 3: make a subroutine that executes each statement in the conditional's then body
    execThenBodySrName = condSrName ++ "_execute_then_body"  
    execThenBodySrDef = execThenBodySrName ++ ":\n" ++                   -- subroutine label
                    concatMap (\(call, _) ->  call) thenBodySrTuples ++  -- concat sequential calls ("\n" and "\t" already in call)  
                    "\tret\n"              

    -- Step 4: if there is an else body, generate the subroutines for each statement in the else body (will be empty if no else body)
    (execElseBodySrName, execElseBodySrDef) = genOptionalElseBody baseName elseBody

    -- Step 5: make a subroutine that will call the conditional eval subroutine and,
    --         if its result stored in rax is true, then call the step 3 subroutine
    condSrCall = "\tcall " ++ condSrName ++ "\n"        -- subroutine call
    condSrDef = condSrName ++ ":\n" ++                  -- subroutine label
                "\tcall " ++ condEvalSrName ++ "\n" ++  -- call the conditional eval subroutine
                "\tcmp rax, 1\n" ++                     -- compare the result of the conditional eval to 1
                "\tje " ++ execThenBodySrName ++ "\n" ++    -- jump to the body execution subroutine if true
                (if not (null elseBody) then "\tjne " ++ execElseBodySrName ++ "\n" else "") ++ -- jump to else body if false
                "\tret\n"                               -- return

    -- Combine all subroutine definitions together
    fullSrDef = condSrDef ++ condEvalSrDef ++ execThenBodySrDef ++ thenBodySrDef ++ execElseBodySrDef 

  in (condSrCall, fullSrDef)

-- Helper function to generate the subroutines for each statement in the else body of a conditional
-- statement. If there is no else body, it will return empty strings. Func works by the same process
-- as is present within genConditionalSr for the then body.
genOptionalElseBody :: String -> [Stmt] -> (String, String)
genOptionalElseBody baseName elseBody = 
  if null elseBody then ("", "")
  else
    let
      -- Generate the subroutines for each statement in the else body
      elseBodySrBaseName = baseName ++ "_else_body_"
      elseBodyIndexed = zipWith (\i stmt -> (elseBodySrBaseName ++ show i, i, stmt)) [0..] elseBody  -- Makes [(name, index, stmt)]
      elseBodySrTuples = map (uncurry3 generateStmtSr) elseBodyIndexed                               -- Generates [(call, def)] for each stmt
      elseBodySrDefs = concatMap snd elseBodySrTuples                                                -- Concatenate all subroutine definitions

      -- Make a subroutine that executes each statement in the conditional's else body  
      execElseBodySrName = baseName ++ "_execute_else_body"
      execElseBodySrDef = execElseBodySrName ++ ":\n" ++      -- Subroutine label
                          concatMap fst elseBodySrTuples ++   -- Concat sequential calls
                          "\tret\n"                           -- Return from subroutine

      -- append the subroutine definitions together
      fullElseBodySrDef = execElseBodySrDef ++ elseBodySrDefs

    in (execElseBodySrName, fullElseBodySrDef)

-------------------------------------------------------------------------------------------------- For Loop Subroutine Generation

genForLoopSr :: String -> Integer -> Stmt -> Expr -> Stmt -> [Stmt] -> (String, String)
genForLoopSr baseName index initStmt condition updateStmt body =
  let
    -- Step 1: generate the subroutine for the initialization statement
    initStmtSrName = baseName ++ "_init_stmt_" ++ show index
    (initStmtSrCall, initStmtSrDef) = generateStmtSr initStmtSrName index initStmt

    -- Step 2: generate the subroutine for the update statement
    updateStmtSrName = baseName ++ "_update_stmt_" ++ show index
    (updateStmtSrCall, updateStmtSrDef) = generateStmtSr updateStmtSrName index updateStmt

    -- Step 3: generate the subroutine for the conditional expression
    (conditionSrDef, conditionSrName, _) = genExprEvalSr baseName index condition

    -- Step 4: generate the subroutine for the body of the for loop
    bodySrBaseName = baseName ++ "_body_" 
    bodyIndexed = zipWith (\i stmt -> (bodySrBaseName ++ show i, i, stmt)) [0..] body -- append index to base name
    bodySrTuples = map (uncurry3 generateStmtSr) bodyIndexed                          -- generate [(call, def)] for each stmt
    bodySrDef = concatMap (\(call, def) -> def) bodySrTuples                          -- concatenate all subroutine definitions into single str

    -- Step 5: generate the subroutine that will call the initialization, condition, and update subroutines
    forLoopSrName = baseName ++ "_for_loop_" ++ show index
    forLoopSrCall = "\tcall " ++ forLoopSrName ++ "\n"
    forLoopSrDef = forLoopSrName ++ ":\n" ++                              -- subroutine label
                  "\tcall " ++ initStmtSrName ++ "\n" ++                 -- call the init stmt subroutine
                  "\tjmp for_loop_condition_" ++ show index ++ "\n" ++   -- jump to the condition check

                  "for_loop_condition_" ++ show index ++ ":\n" ++        -- label for the condition check
                  "\tcall " ++ conditionSrName ++ "\n" ++                -- call the condition eval subroutine
                  "\tcmp rax, 0\n" ++                                    -- compare the result of the condition eval to 0
                  "\tje for_loop_end_" ++ show index ++ "\n" ++          -- jump to end of loop if condition is false (zero)

                  "\tcall " ++ bodySrBaseName ++ "0\n" ++                -- call the body of the for loop
                  "\tcall " ++ updateStmtSrName ++ "\n" ++               -- call the update stmt subroutine
                  "\tjmp for_loop_condition_" ++ show index ++ "\n" ++   -- jump back to condition check

                  "for_loop_end_" ++ show index ++ ":\n" ++              -- label for end of loop
                  "\tret\n"

    -- Combine all subroutine definitions together
    fullSrDef = forLoopSrDef ++ initStmtSrDef ++ conditionSrDef ++ updateStmtSrDef ++ bodySrDef

  in (forLoopSrCall, fullSrDef)


-------------------------------------------------------------------------------------------------- While Loop Subroutine Generation



genWhileLoopSr :: String -> Integer -> Expr -> [Stmt] -> (String, String) 
genWhileLoopSr baseName index condition body =
  let
    
    -- Step 1: generate subroutine for the conditional expression
    (conditionSrDef, conditionSrName, _) = genExprEvalSr baseName index condition

    -- Step 2: generate the subroutine for the body of the for loop
    bodySrBaseName = baseName ++ "_body_" 
    bodyIndexed = zipWith (\i stmt -> (bodySrBaseName ++ show i, i, stmt)) [0..] body -- append index to base name
    bodySrTuples = map (uncurry3 generateStmtSr) bodyIndexed                          -- generate [(call, def)] for each stmt
    bodySrDef = concatMap (\(call, def) -> def) bodySrTuples                          -- concatenate all subroutine definitions into single str

    -- Step 3: generate subroutine 
    whileLoopSrName = baseName ++ "_while_loop_" ++ show index
    whileLoopSrCall = "\tcall " ++ whileLoopSrName ++ "\n"
    whileLoopSrDef = whileLoopSrName ++ ":\n" ++                           -- subroutine label
                  "\tjmp while_loop_condition_" ++ show index ++ "\n" ++   -- jump to the condition check

                  "while_loop_condition_" ++ show index ++ ":\n" ++        -- label for the condition check
                  "\tcall " ++ conditionSrName ++ "\n" ++                  -- call the condition eval subroutine
                  "\tcmp rax, 0\n" ++                                      -- compare result of the condition eval to 0
                  "\tje while_loop_end_" ++ show index ++ "\n" ++            -- jump to end of loop if condition is false (zero)
                  "\tcall " ++ bodySrBaseName ++ "0\n" ++                  -- call the body of the for loop
                  "\tjmp while_loop_condition_" ++ show index ++ "\n" ++   -- jump back to condition check

                  "while_loop_end_" ++ show index ++ ":\n" ++              -- label for end of loop
                  "\tret\n"

    -- Combine all subroutine definitions together
    fullSrDef = whileLoopSrDef ++ conditionSrDef ++ bodySrDef

  in (whileLoopSrCall, fullSrDef)




-------------------------------------------------------------------------------------------------- Increment/Decrement Subroutine Generation

-- Generates subroutine definition and call for incrementing a variable
genIncrementSr :: String -> Integer -> String -> (String, String)
genIncrementSr baseName index varName = 
  let
    -- Set unique subroutine names
    incrementSrName = baseName ++ show index

    -- Set the subroutine definition for the increment statement
    incrementSrCall = "\tcall " ++ incrementSrName ++ "\n"
    incrementSrDef = incrementSrName ++ ":\n" ++
                     "\tinc [" ++ varName ++ "_label]\n" ++  -- Increment the value of the variable
                     "\tret\n"                               -- Return from subroutine

  in (incrementSrCall, incrementSrDef)


-- Generates subroutine defintion and call for decrementing a variable
genDecrementSr :: String -> Integer -> String -> (String, String)
genDecrementSr baseName index varName = 
  let
    -- Set unique subroutine names
    decrementSrName = baseName ++ show index

    -- Set the subroutine definition for the decrement statement
    decrementSrCall = "\tcall " ++ decrementSrName ++ "\n"
    decrementSrDef = decrementSrName ++ ":\n" ++
                     "\tdec [" ++ varName ++ "_label]\n" ++  -- Decrement the value of the variable
                     "\tret\n"                               -- Return from subroutine

  in (decrementSrCall, decrementSrDef)

-------------------------------------------------------------------------------------------------- Expression Evaluation Subroutine generation

-- genExprSr recursively chains subroutine definitions together for evaluating expressions in a depth first manner. 
-- Literal/Variable expressions are the base case. Unary/Binary expressions will result in recursively chaining of
-- subroutines to handle the evaluation of the subexpressions.
-- @param The base name of expression evaluation subroutine, the number of subroutines that have already been chained 
-- from the base to get to this one, and the current expr/subexpr.
genExprEvalSr :: String -> Integer -> Expr -> (String, String, Integer)
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
literalEvalSr :: String -> Integer -> Show a => a -> String -> (String, String, Integer)
literalEvalSr baseName index value typeName =
  let
    -- set unique subroutine name
    subroutineName = baseName ++ "_" ++ show index

    -- set subroutine definition
    subroutineDef = subroutineName ++ ":\n" ++
                    "\tmov rax, " ++ show value ++ "\n" ++
                    "\tret\n"
  in (subroutineDef, subroutineName, index) -- return subroutine def, name, updated idx


-- Helper func to generate subroutnie that moves the value of a variable into rax during expression eval
variableEvalSr :: String -> Integer -> String -> (String, String, Integer)
variableEvalSr baseName index varName =
  let
      -- set unique subroutine name derived from base name, varName, and index
      subroutineName = baseName ++ "_" ++ show index

      -- set subroutine definition 
      subroutineDef = subroutineName ++ ":\n" ++                   -- subroutine label
                      "\tmov rax, [" ++ varName ++ "_label]\n" ++  -- move value of varName into rax
                      "\tret\n"                                    -- return from subroutine

    in (subroutineDef, subroutineName, index) -- return the subroutine def, name, updated idx

-- Helper func to generate subroutine that evaluates a unary operation recursively by chaining
-- subroutine definitions for evaluating all subexpressions together. The output of the subexpression 
-- is stored in the rax register the unary operation is applied.
unaryOpEvalSr :: String -> Integer -> UnaryOp -> Expr -> (String, String, Integer)
unaryOpEvalSr baseName index op subExpr =
  let
      -- recursively evaluate the subexpression. NOTE that the index passed in is incremented
      (subExprDef, subExprName, newIndex) = genExprEvalSr baseName (index + 1) subExpr

      -- set unique subroutine name derived from base name, unary op, and index
      subroutineName = baseName ++  "_" ++ show index

      -- set subroutine definition 
      subroutineDef = subroutineName ++ ":\n" ++           -- subroutine label
                      "\tcall " ++ subExprName ++ "\n" ++  -- call the subexpression eval subroutine
                      unaryOpAsm op ++                     -- use unaryOpAsm to generate instructions for specific op
                      "\tret\n"                            -- return from subroutine

      -- append the subExprDef to the subroutine definition
      fullSubroutineDef = subroutineDef ++ subExprDef

    -- return the full subroutine def, name, updated idx
    in (fullSubroutineDef, subroutineName, newIndex)

-- Helper func to generate a subroutine that evaluates a binary operation recursively by 
-- generating a chain of subroutine definitions that will evaluate the left and right hand
-- subexpressions in a depth first manner. The output of the left hand side expression is
-- stored on the stack before evaluating the rhs. The final result is stored in the rax register.
binaryOpEvalSr :: String -> Integer -> Op -> Expr -> Expr -> (String, String, Integer)
binaryOpEvalSr baseName index op lhs rhs =
  let

      -- Generate subroutine definitions for evaluating LHS and RHS
      (lhsExprEvalSrDef, lhsExprEvalSrName, lhsLastIndex) = genExprEvalSr (baseName ++ "_lhs_eval") (index) lhs
      (rhsExprEvalSrDef, rhsExprEvalSrName, rhsLastIndex) = genExprEvalSr (baseName ++ "_rhs_eval") (lhsLastIndex) rhs

      -- Unique subroutine name for the binary operation
      binaryOpSrName = baseName ++ "_" ++ show index

      -- Define the binary operation subroutine
      binaryOpSrDef = binaryOpSrName ++ ":\n" ++

          -- Evaluate the LHS expression and store the result temporarily
          "\tcall " ++ lhsExprEvalSrName ++ "\n" ++
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

    -- Return the complete binary operation sr def, name, updated idx
    in (fullBinaryOpSrDef, binaryOpSrName, rhsLastIndex)


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

  -- Arithmetic operations
  Add -> "\tadd rax, rbx\n"
  Subtract -> "\tsub rax, rbx\n"
  Multiply -> "\timul rax, rbx\n"
  Divide -> "\tcqo\n" ++ "\tidiv rbx\n"  -- Sign-extend rax into rdx:rax before division

  -- Comparison operations
  LessThan -> "\tcmp rax, rbx\n" ++ "\tsetl al\n" ++ "\tmovzx rax, al\n"
  GreaterThan -> "\tcmp rax, rbx\n" ++ "\tsetg al\n" ++ "\tmovzx rax, al\n"
  LessEq -> "\tcmp rax, rbx\n" ++ "\tsetle al\n" ++ "\tmovzx rax, al\n"
  GreaterEq -> "\tcmp rax, rbx\n" ++ "\tsetge al\n" ++ "\tmovzx rax, al\n"
  Equal -> "\tcmp rax, rbx\n" ++ "\tsete al\n" ++ "\tmovzx rax, al\n"
  NotEqual -> "\tcmp rax, rbx\n" ++ "\tsetne al\n" ++ "\tmovzx rax, al\n"

  -- Logical operations
  And -> "\tand rax, rbx\n" ++ "\ttest rax, rax\n" ++ "\tsetnz al\n" ++ "\tmovzx rax, al\n"
  Or -> "\tor rax, rbx\n" ++ "\ttest rax, rax\n" ++ "\tsetnz al\n" ++ "\tmovzx rax, al\n"

  _ -> error "Operation not supported"
