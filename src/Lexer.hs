module Lexer (Token(..), lexer) where

-- Lexer.hs: This module defines the tokens and 
-- the lexer function for a basic C-like language.

import Data.Char (isDigit, isAlpha, isSpace, isAlphaNum)

-- This module defines the tokens that the lexer will recognize
-- The lexer function will convert a string into a list of Tsokens
data Token =

  -- main/functions
  TMain | TReturn |

  -- Data types
  TInt | TChar | TDouble | TFloat | TVoid

  -- Literals and identifiers
  | TIntLit Int | TDoubleLit Double | TIdent String  | TFloatLit Float | TCharLit Char 

  -- Operators
  | TAssign | TPlus | TMinus | TStar | TSlash | TPercent | TEqual | TNotEqual 
  | TPlusAssign | TMinusAssign | TMultAssign | TDivAssign | TModAssign
  | TIncrement | TDecrement

  -- Logical ops and delimiters  
  | TAnd | TOr | TNot | TLparen | TRparen | TLbrace | TRbrace

  -- Punctuation  
  | TSemicolon | TComma 

  -- Control Structures
  | TIf | TElse | TFor | TWhile 
 
  -- Comparison operators
  | TLessThan | TGreaterThan | TLessEq | TGreaterEq 

  -- End of file/input token
  | TEOF  

  deriving (Show, Eq) -- Make Token type printable and comparable

-- The lexer function will convert a string into a list of tokensby recursively processing the 
-- string, one char at a time. As tokens are recognized, they are appended to the list
lexer :: String -> [Token]
lexer [] = [TEOF] -- base case: end of file/input
lexer (c:cs)
  | isSpace c = lexer cs       -- Skip whitespace
  | isDigit c = lexNumber c cs -- Handle numbers
  | isAlpha c = lexIdent c cs  -- Handle identifiers

  -- overloaded assignment operators
  | c == '+' && not (null cs) && head cs == '=' = TPlusAssign : lexer (tail cs)
  | c == '+' && not (null cs) && head cs == '+' = TIncrement : lexer (tail cs)
  | c == '-' && not (null cs) && head cs == '=' = TMinusAssign : lexer (tail cs)
  | c == '-' && not (null cs) && head cs == '-' = TDecrement : lexer (tail cs)
  | c == '*' && not (null cs) && head cs == '=' = TMultAssign : lexer (tail cs)
  | c == '/' && not (null cs) && head cs == '=' = TDivAssign : lexer (tail cs)
  | c == '%' && not (null cs) && head cs == '=' = TModAssign : lexer (tail cs)

  -- arithmetic operators
  | c == '+' = TPlus : lexer cs
  | c == '-' = TMinus : lexer cs
  | c == '*' = TStar : lexer cs
  | c == '/' = TSlash : lexer cs
  | c == '%' = TPercent : lexer cs

  -- punctuation 
  | c == ';' = TSemicolon : lexer cs
  | c == ',' = TComma : lexer cs
  | c == '(' = TLparen : lexer cs
  | c == ')' = TRparen : lexer cs
  | c == '{' = TLbrace : lexer cs
  | c == '}' = TRbrace : lexer cs

  -- logical/comparison operators
  | c == '=' && not (null cs) && head cs == '=' = TEqual : lexer (tail cs)
  | c == '=' = TAssign : lexer cs
  | c == '<' && not (null cs) && head cs == '=' = TLessEq : lexer (tail cs)
  | c == '<' = TLessThan : lexer cs
  | c == '>' && not (null cs) && head cs == '=' = TGreaterEq : lexer (tail cs)
  | c == '>' = TGreaterThan : lexer cs
  | c == '!' && not (null cs) && head cs == '=' = TNotEqual : lexer (tail cs)
  | c == '!' = TNot : lexer cs
  | otherwise = lexer cs  -- Skip unrecognized characters

-- lexNumber function handles the lexing of numbers. It is called within the lexer function
-- and is passed the current character being processed and the rest of the string following it
-- It returns a list of tokens that represent the number.
lexNumber :: Char -> String -> [Token] 
lexNumber c cs

    -- If the number contains a decimal point, it is a double literal else int literal
    -- The full num is processed using the where clause and converted to a token. The 
    -- read function is used to convert the string to a number.
  | '.' `elem` num = TDoubleLit (read num) : lexer rest
  | otherwise = TIntLit (read num) : lexer rest

    -- This 'where clause' will split the full number from the rest of the string. The remaining 
    -- string is recursively passed back to the lexer function for further processing. 
  where (num, rest) = span (\x -> isDigit x || x == '.') (c:cs)

-- lexIdent function handles the lexing of identifiers (keywords, variable names, etc.)
lexIdent :: Char -> String -> [Token]
-- use the where clause to split the full identifier from the rest of the string
-- determining which token to return before recursively calling the lexer function
lexIdent c cs = case ident of
    "int" -> TInt : lexer rest
    "char" -> TChar : lexer rest
    "double" -> TDouble : lexer rest
    "float" -> TFloat : lexer rest
    "if" -> TIf : lexer rest
    "else" -> TElse : lexer rest
    "for" -> TFor : lexer rest
    "while" -> TWhile : lexer rest
    "main" -> TMain : lexer rest
    "void" -> TVoid : lexer rest
    "return" -> TReturn : lexer rest
    _ -> TIdent ident : lexer rest

  -- This 'where clause' will split the full identifier from the rest of the string. The remaining
  -- string is recursively passed back to the lexer function for further processing.
  where (ident, rest) = span isAlphaNum (c:cs)
