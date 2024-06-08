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
    (typeMap, declarationStmts) = getTypeMap program

    -- filter declarations into .data and .bss sections
    (dataDecls, bssDecls) = filterDeclarations declarationStmts

    -- generate the .data section
    dataSection = genDataSection dataDecls

    -- generate the .bss section
    bssSection = genBssSection bssDecls typeMap

  in
    show typeMap ++ "\n" ++
    show declarationStmts ++ "\n" ++
    "Data Declarations: " ++ show dataDecls ++ "\n" ++
    "Bss Declarations: " ++ show bssDecls ++ "\n"++ "\n\n\n" ++
    dataSection ++
    bssSection ++ "\n"




-- Writes the assembly code to a file
writeToFile :: FilePath -> String -> IO ()
writeToFile path content = writeFile path content

