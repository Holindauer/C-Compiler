module Parser_Expressions where

import Parser_Helper
import Lexer
import AST
import Debug.Trace (traceShow, trace)


-- Parser_Expressions.hs contains the functions for parsing expressions in the program.
-- The parseExpr function is the main entry point for parsing expressions.

-------------------------------------------------------------------------------------------------- Expression Parsing

-- parseExpr handles expressions by determining if they are simple, unary, or binary expressions based on the context.
-- It returns a ParserResult that contains either the parsed expression and the remaining tokens or an error.
parseExpr :: LexedTokens -> ParserResult (Expr, LexedTokens)
parseExpr tokens = case tokens of

    -- Check for parenthesized expressions first.
    (TLparen : _) -> parseParenthesizedExpr tokens

    -- If not a parenthesized expression, attempt to parse as a primary expression.
    _ -> parsePrimaryExpr tokens >>= \(expr, remainingTokens) ->
            case remainingTokens of

                -- If no more tokens, it's a primary expression.
                [] -> Right (expr, remainingTokens)

                -- If more tokens, check if it's an operator to decide on binary expression.
                (nextToken : _) -> 
                    if isOperator nextToken
                    then continueParsingBinaryExpr expr remainingTokens
                    else Right (expr, remainingTokens)

-- Parses parenthesized expressions ensuring correct closure of parentheses.
parseParenthesizedExpr :: LexedTokens -> ParserResult (Expr, LexedTokens)
parseParenthesizedExpr (TLparen : rest) = 

    -- Parse the expression inside the parentheses
    parseExpr rest >>= \(expr, afterExpr) ->
        case afterExpr of
            (TRparen : remainingTokens) -> Right (expr, remainingTokens)
            _ -> Left (InvalidSyntax "Expected closing parenthesis")
parseParenthesizedExpr _ = Left (InvalidSyntax "Expected opening parenthesis")

-- Primary expression parsing.
parsePrimaryExpr :: LexedTokens -> ParserResult (Expr, LexedTokens)
parsePrimaryExpr [] = Left UnexpectedEndOfInput
parsePrimaryExpr (token : rest) = case token of

    -- Parse literals and variables as primary expressions.
    TIntLit i -> Right (IntLit (toInteger i), rest)
    TFloatLit f -> Right (FloatLit f, rest)
    TDoubleLit d -> Right (DoubleLit d, rest)
    TIdent var -> Right (Var var, rest)
    _ -> Left (InvalidSyntax "parsePrimaryExpr: Invalid primary expression")

-- continueParsingBinaryExpr is called within parseExpr to continue an expression that is determined to
-- be a binary expression. It returns a ParserResult that contains either the parsed expression and the
-- remaining tokens or an error.
continueParsingBinaryExpr :: Expr -> LexedTokens -> ParserResult (Expr, LexedTokens)
continueParsingBinaryExpr leftExpr (operator : rest) = 
    parseExpr rest >>= \(rightExpr, remainingTokens) ->
        let op = tokenToOperator operator in
        Right (BinOp op leftExpr rightExpr, remainingTokens)

-- Helper function to convert a token to an operator.
tokenToOperator :: Token -> Op
tokenToOperator token = case token of
    TGreaterThan -> GreaterThan
    TLessThan -> LessThan
    TEqual -> Equal
    TNotEqual -> NotEqual
    TLessEq -> LessEq
    TGreaterEq -> GreaterEq
    TPlus -> Add
    TMinus -> Subtract
    TStar -> Multiply
    TSlash -> Divide
    TPercent -> Modulus
    _ -> error "Unsupported operator"

-- Checks if a token is an operator.
isOperator :: Token -> Bool
isOperator token = token `elem` [TPlus, TMinus, TStar, TSlash, TPercent, TGreaterThan, TLessThan, TEqual, TNotEqual, TLessEq, TGreaterEq]

