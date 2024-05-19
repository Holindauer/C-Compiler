{-# LANGUAGE LambdaCase #-}

-- Parser_Main.h contains the high level fascilitation for parsing the list of tokens 
-- output by the lexer into abstract syntax trees for each statement in the program.
-- Function defs for parsing statements and expressions evals are in Parser_Expressions.hs
-- and Parser_Statements.hs respectively.

module Parser_Main where

import Parser_Statements
import Parser_Helper
import Lexer
import AST
import Debug.Trace (traceShow, trace)


-- parsePogram is the master parsing function that parses the entire program by recursively 
-- processing each statement, packaging them into the AST and returning the final result.
-- It accepts the list of tokens received from the lexer and returns a ParserResult
-- @dev Left and Right are constructors of the Either type
parseProgram :: LexedTokens -> ParserResult ([Stmt], LexedTokens)
parseProgram [] = Right ([], []) -- Empty program

-- Parse into Main function
parseProgram (TInt : TMain : TLparen : TVoid : TRparen : TLbrace : tokens) = 

    -- parse and return the interior of the main function
    -- there should be no tokens left after parsing the main function
    parseProgram tokens >>= \(stmts, remainingTokens) ->
        if null remainingTokens
        then Right (stmts, remainingTokens)
        else Left (InvalidSyntax "Unexpected tokens after main function")
        
-- parse interior of main func following 'int main(void) {'
parseProgram tokens = do
    -- Parse the next statement and recursively call parseProgram
    case parseStmt tokens of     
        Left err -> Left err     
        Right (Nothing, rest) -> parseProgram rest -- Skip empty statements
        Right (Just stmt, rest) ->
            -- Recursively parse the remaining tokens
            case parseProgram rest of
                Left err -> Left err
                Right (stmts, remainingTokens) -> Right (stmt : stmts, remainingTokens)


