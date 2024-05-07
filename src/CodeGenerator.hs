module CodeGenerator where

import System.IO
import AST
import Data.List (intercalate, foldl')
import qualified Data.Map as Map

-- Writes the assembly code to a file
writeToFile :: FilePath -> String -> IO ()
writeToFile path content = writeFile path content

-- Function to generate the .bss section text only
generateBssSection :: [Stmt] -> String
generateBssSection stmts = foldl' generateBssText "" stmts -- foldl' == left fold, processes each stmt in list from left to right
  where
    generateBssText bssAccumulator stmt = case stmt of

      -- reserve stack space for uninitialized variables at declaration
      SimpleDeclaration dataType (Var varName) -> 
        let
          label = varName ++ "_label"    -- set the label for the variable
          size = dataTypeToSize dataType -- get the size of the data type
        in bssAccumulator ++ label ++ ": " ++ size ++ "\n"
     
      -- reserve stack space for vars w/ intial values at declaration. Note that 
      -- the initial value will be set where the declaration is made sequentially,
      -- Here we are just allocating memory for later.
      DeclarationAssignment dataType varName _ ->
        let
          label = varName ++ "_label"
          size = dataTypeToSize dataType
        in bssAccumulator ++ label ++ ": " ++ size ++ "\n" 
        
      _ -> bssAccumulator  -- Ignore non-declaration statements

-- Helper to convert data type to .bss reservation size
-- This is specific to the NASM syntax
dataTypeToSize :: String -> String
dataTypeToSize dataType = case dataType of
  "int"    -> "resd 1"  -- Reserve space for one double-word (4 bytes)
  "float"  -> "resd 1"
  "double" -> "resq 1"  -- Reserve space for one quad-word (8 bytes)
  "char"   -> "resb 1"  -- Reserve space for one byte
  _        -> error "Unsupported data type"

-- Example usage of the function
-- This can be adapted based on how your AST and statement types are structured.
generateCode :: Program -> String
generateCode program =
  let
    -- generate .bss section (reservation of stack space for variables)
    bssSection = generateBssSection program
    
  in
    "section .bss\n" ++ bssSection ++ "section .text\n"