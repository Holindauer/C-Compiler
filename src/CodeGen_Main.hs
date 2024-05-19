{-# LANGUAGE RankNTypes #-}


module CodeGen_Main where

import System.IO
import AST
import CodeGen_Statements
import CodeGen_Expressions
import Helper
import Data.List (foldl', zipWith)
import Debug.Trace (traceShow, trace)
import qualified Data.Map as Map

-- Code Generator Design Summary:
--
-- The code generator transforms parsed statements into NASM x86-64 assembly code following a structured approach:
-- 1. Stack Space Allocation: Calculates and allocates necessary stack space for variables in the .bss section,
--    using names from the source file for clarity and correspondence.
-- 2. Translation of Statements: Converts each parsed statement into NASM assembly subroutines. These subroutines,
--    when executed in sequence from the _start entry point, represent the program's execution flow.
-- 3. Subroutine Types: Handles different types of statements such as Assignments, Expressions, Conditionals,
--    and Loops (For and While). Assignments alter program state, while other types manage control flow and computations.
-- 4. Assignment Translation: Involves evaluating the right-hand side expression into a register and then storing this
--    value in the appropriate .bss section variable.
-- 5. Expression Evaluation: Handles literals and variables directly; processes unary and binary operations by
--    recursively chaining subroutines based on the AST's structure, resulting in a depth-first evaluation that outputs
--    to an intermediate register.
-- 6. Conditional and Loop Handling: Utilizes a recursive evaluation of conditions and manages loops by repeatedly
--    executing the loop body until the end condition is satisfied, following the same depth-first strategy for any
--    internal expressions or nested statements.
--
-- This design ensures a systematic translation of high-level constructs to low-level assembly instructions, facilitating
-- efficient and organized code generation.
--
-- NOTE: CodeGen_main.hs contains the implementaion of the fascilitation of the above processes. The impl of the generation of
-- expresion eval subroutines is within CodeGen_Expressions.hs, and the impl of the generation of statement eval subroutines is
-- within CodeGen_Statements.hs.

-------------------------------------------------------------------------------------------------- Code Generation

-- generateCode is the master function for generating NASM assembly code from a parsed program. It generates the .bss 
-- and .text sections by calling generateBssSection and generateTextSection. Then packages them into a full asm program
generateCode :: Program -> String
generateCode program =
  let
    -- generate .bss and .text 
    bssSection = generateBssSection program
    textTuples = generateTextSection program 

    -- concat defs and calls of .text section into single strings respectively
    textSrDefs = concatMap (\(call, def) -> def) textTuples 
    textSrCalls = concatMap (\(call, def) -> call) textTuples 

    -- assemble _start entry point for the program
    startSection = 
      -- set global _start and text section
      "global _start\n" ++ "section .text\n\n" ++ 
      
      -- _start def w/ calls to all subroutines
      "_start:\n" ++ textSrCalls ++

      -- exit program w/ syscall 60, exist status 0
	    "\tmov rax, 60 \n" ++ "\txor rdi, rdi\n" ++ "\tsyscall\n" 
  in
    "section .bss\n" ++ bssSection ++ "\n" ++ startSection ++ "\n" ++ textSrDefs

-- Writes the assembly code to a file
writeToFile :: FilePath -> String -> IO ()
writeToFile path content = writeFile path content

-------------------------------------------------------------------------------------------------- .bss section generation

-- generateBssSection performs a left fold over the list of statements returned by the parser, locating all variable 
-- declarations at all levels of indentation within the program, and generates the corresponding .bss section accordingly
-- @dev the BSS section is where uninitialized data is allocated in memory at compile time 
generateBssSection :: [Stmt] -> String
generateBssSection stmts = foldl' generateBssText "" stmts -- left fold over stmt list into an empty string
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
        generateBssSection body ++ bssAccumulator  -- recurse for while loop body

      -- If-Else statement body
      IfStmt _ thenBody elseBody ->
        let 
            thenBss = generateBssSection thenBody -- recurse for then,else bodies
            elseBss = generateBssSection elseBody 
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
