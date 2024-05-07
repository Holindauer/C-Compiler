module CodeGenerator where

import System.IO
import AST
import Data.List (foldl')
import qualified Data.Map as Map


-- Master code generation function
generateCode :: Program -> String
generateCode program =
  let
    bssSection = generateBssSection program
  in
    "section .bss\n" ++ bssSection ++ "section .text\n"


-- Writes the assembly code to a file
writeToFile :: FilePath -> String -> IO ()
writeToFile path content = writeFile path content

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

