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
-- variable names that match that which is found in the source file. Then to transalte each statement into a series 
-- of NASM assembly instructions that use the allocated stack space to store the output of computation.
--
-- A subroutine of instructions for each statement in the program will be collected into the _start entry point for the
-- program.  Thus the _start entry point will contain sequential calls to subroutines that correspond to each statement
-- AST node in the collected statements list. 
--
-- There are four types of subroutine template that will be filled in with the specific information relavant to the 
-- operation being performed in the statement and called within the _start entry point:
--
--   1. Assignment 
--   2. Conditional
--   3. For Loop
--   4. While Loop
--
-- It should be noted that only Assignment subroutines will change the state of the program. The other subroutines will 
-- determine control flow and repetition of other subroutines.
--
-- As Assignment statement will be transalted into the text of a subroutine that contains two steps. The first is to evaluate
-- the expression on the right hand side of the statement into an intermediate value register. The second step is to store 
-- the value that was placed into the intermediate register into the appropritate l-value variable name of which memory was 
-- allocated for in the .bss section
--
-- Now we must discuss how expressions are to be allocate. When an l-value expression is encounted, a seperate subroutine must
-- be written that will recursively chain subroutines together based on the grammatical structure that was preserved in the
-- AST. This means that the expression will be evaluated in a depth first manner.
--
-- There are four cases for what can be assigned to a variable:
--
--   1. A literal value
--   2. Another variable's value
--   3. The evaluation of a unary operation
--   4. The evaluation of a binary operation
--
-- In the context of the depth first subroutine chain, an expression that is a literall value or another variable is considered
-- to be the base case. If this is the case, the value of the literal or variable is moved into an intermediate register for the 
-- next depth up to use in their assignment or expression evalulation. 
-- 
-- If the expression is a unary operation, two steps will ensue. The first is that the expression that is the argument of the unary
-- operation will be evaluated by calling another subroutine that will place the evaluation into an intermediate output register.
-- Then, the output in that register will have the unary operation applied to it and returned to an output register as well.
-- This is to say that everytime one of the 4 expression possibilities is encountered within the depths of a statement evaluation, 
-- a further subroutine will be created to handle and evaluate it, with literal and variable expression being the base case. This 
-- forms a sort of pseudo-recursive chain of subroutines that evaluate depth first the entirety of the expression.
--
-- If the expression is a binary operation, the same process will occur as with the unary operation, but with the added step of
-- allocating dynamic space for the intermediate outputs of the lhs expr and rhs expr. This is to ensure that the same output
-- register can be used for the final output without having to worry about overwriting the intermediate values. First the lhs
-- will be evaluated, then the rhs, similar to an in order traversal of a binary tree. The outputs collected into the dynamically
-- allocated space will then be used as the arguments of the specific operation that makes up the binary operation. The output of
-- this computation will be placed into the output register and returned to the calling subroutine.
--
-- after the depth first evaluated is complete, the value will be moved into the variable declared in the .bss section that corresponds
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

      -- Simple variable declaration
      SimpleDeclaration dataType (Var varName) -> 
        appendBss bssAccumulator dataType varName
      
      -- Variable declaration with assignment
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
generateTextSection stmts = map (uncurry generateStmtSubroutine) indexedStmts -- uncurry unpacks tuple
  where
    -- zipWith is used to pair each statement with its index so to create unique subroutine names
    indexedStmts = zipWith (,) [0..] stmts
    

-- generateStmtSubroutine creates the subroutine call and definition for a single statement.
-- @param the statement number and the statement itself
generateStmtSubroutine :: Integer -> Stmt -> (String, String)
generateStmtSubroutine stmtNum stmt = case stmt of

  -- literal assignment
  AssignStmt varName (IntLit value) -> literalAssignmentSubroutine stmtNum varName (show value)
  AssignStmt varName (FloatLit value) -> literalAssignmentSubroutine stmtNum varName (show value)
  AssignStmt varName (DoubleLit value) -> literalAssignmentSubroutine stmtNum varName (show value)
  AssignStmt varName (CharLit value) -> literalAssignmentSubroutine stmtNum varName (show value)

  -- variable assignment
  AssignStmt lValue (Var rValue) -> variableAssignmentSubroutine lValue rValue


  _ -> error "Unsupported statement type"


-- literalAssignmentSubroutine generates the NASM assembly code for an assignment of a literal value
-- to a variable that has been preinitialized within the .bss section. The generated subroutine does
-- not require any arguments as the value to be assigned is hardcoded into the subroutine. As such, the
-- subroutine will only contain the instructions to move the hardcoded value into the variable's memory
-- The varable is the name of the variable it was assignbed to + "_label" and the value is the literal
-- value that was assigned to it
-- literalAssignmentSubroutine accepts the variable name, the literal value and an integer representing the
-- number of subroutines that have already been generated. The integer is to ensure that all subroutines 
-- have unique names
-- literalAssignmentSubroutine will return a tuple containing the call to the subroutine and the subroutine
-- definition itself
literalAssignmentSubroutine :: Integer -> String -> String -> (String, String)
literalAssignmentSubroutine stmtNum varName literalValue =
  let
    subroutineName = varName ++ "_literal_assignment_" ++ show stmtNum
    subroutineCall = "\tcall " ++ subroutineName ++ "\n"
    subroutineDefinition = subroutineName ++ ":\n" ++
      "\tmov rax, " ++ literalValue ++ "\n" ++
      "\tmov [" ++ varName ++ "_label], rax\n" ++
      "\tret\n"
  in (subroutineCall, subroutineDefinition)


  -- placeholder for variableAssignmentSubroutine
variableAssignmentSubroutine :: String -> String -> (String, String)
variableAssignmentSubroutine lValue rValue =
  let
    subroutineName = lValue ++ "_to_" ++ rValue
    subroutineCall = "\tcall " ++ subroutineName ++ "\n"
    subroutineDefinition = subroutineName ++ ":\n" ++
      "\tmov rax, [" ++ rValue ++ "_label]\n" ++
      "\tmov [" ++ lValue ++ "_label], rax\n" ++
      "\tret\n"
  in (subroutineCall, subroutineDefinition)
