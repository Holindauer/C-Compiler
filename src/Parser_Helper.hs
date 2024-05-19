module Parser_Helper where

import Lexer

-- Type alias for lexed tokens 
type LexedTokens = [Token]

-- ParserResult type alias. Parsing results in either a ParseError or a value of type a
type ParserResult a = Either ParseError a

-- ParseError type is used to represent potential errors that can occur during parsing
data ParseError = UnexpectedToken Token
                | MissingSemicolon
                | InvalidSyntax String
                | UnexpectedEndOfInput
                deriving (Show, Eq) 