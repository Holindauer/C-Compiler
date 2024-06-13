module CodeGen_TextSection where

import AST
import CodeGen_Declarations
import CodeGen_Statements
import CodeGen_Helper

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Hashable

-- genTextSection returns a list of tuples containing subroutine calls and their definitions
genTextSection :: [Stmt] -> TypeMap -> FloatMap -> String
genTextSection stmts typeMap floatMap = 
  let 
    -- Generate tuples of subroutine calls and definitions from statements
    textTuples = map (\stmt -> uncurry3 generateStmtSr stmt typeMap floatMap) indexedStmts

    -- Concatenate definitions and calls of .text section into single strings respectively
    textSrDefs = concatMap (\(_, def) -> def) textTuples 
    textSrCalls = concatMap (\(call, _) -> call) textTuples 

    -- package text section into _start entry 
    startSection = 
      -- set global _start and text section
      "global _start\nsection .text\n\n" ++ 
      
      -- _start def w/ calls to all subroutines
      "_start:\n" ++ textSrCalls  ++ "\n\n" ++

      -- subroutine definitions
      textSrDefs

  in startSection 

  where
    -- Create list of empty strings the same length as stmts
    emptyStrings = replicate (length stmts) ""
    indexedStmts = zip3 emptyStrings [0..] stmts   -- ("", i, stmt)

    -- uncurry for 3 args
    uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
    uncurry3 f (x, y, z) = f x y z



