{-# LANGUAGE RankNTypes #-}


module CodeGen_Main where

import CodeGen_Declarations

import System.IO
import AST
import Data.List (foldl', zipWith)
import Debug.Trace (traceShow, trace)
import qualified Data.Map as Map


-- -------------------------------------------------------------------------------------------------- Code Generation

-- generateCode is the master function for generating NASM assembly code from a parsed program. 
generateCode :: Program -> String
generateCode program =
  let
    -- retrieve all types in the program
    typeMap = getTypeMap program

  in
    show typeMap



-- Writes the assembly code to a file
writeToFile :: FilePath -> String -> IO ()
writeToFile path content = writeFile path content

