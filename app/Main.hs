module Main (main) where

import System.Environment (getArgs)
import Lexer
import AST
import Parser_Main
import CodeGen_Main


main :: IO ()
main = do
  args <- getArgs
  if length args < 2 then
    putStrLn "Usage: runhaskell Main.hs <input file> <output file>"
  else do

    -- read src file contents
    let filePath = head args
    let outputPath = args !! 1
    content <- readFile filePath

    -- lex contents of file
    let tokens = lexer content
    putStrLn $ "\nTokens: \n" ++ show tokens ++ "\n"

    -- parse the lexed tokens
    let programResult = parseProgram tokens
    putStrLn $ "\nParsed Statements: \n" ++ show programResult ++ "\n"

    -- generate code from parsed program
    case programResult of

      -- gen code and write to file
      Right (program, _) -> do
        let assemblyCode = generateCode program
        writeToFile outputPath assemblyCode
        putStrLn $ "Code generation complete. Assembly file " ++ outputPath ++ " written."

      -- Error
      Left parseError -> putStrLn $ "Parse error: " ++ show parseError