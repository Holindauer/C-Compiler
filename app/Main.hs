module Main (main) where

import System.Environment (getArgs)
import Lexer
import AST
import Parser
import CodeGenerator

main :: IO ()
main = do
  args <- getArgs
  if length args < 2 then
    putStrLn "Usage: runhaskell Main.hs <input file> <output file>"
  else do
    let filePath = head args
    let outputPath = args !! 1
    content <- readFile filePath
    let tokens = lexer content

    -- Handle the Either type returned by parseProgram
    let programResult = parseProgram tokens
    case programResult of
      Left parseError -> putStrLn $ "Parse error: " ++ show parseError
      Right (program, _) -> do
        let assemblyCode = generateCode program
        writeToFile outputPath assemblyCode
        putStrLn $ "Code generation complete. Check " ++ outputPath ++ " for the generated code."