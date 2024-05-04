{-# LANGUAGE LambdaCase #-}

-- Parser.h contains parser functions for the C subset langauge.  The parser is used after 
-- the Lexer, converting the outputted list of tokens into an bstract syntax tree (defined
-- in AST.hs) that represents the program's gramatical structure.

module Parser 
  ( parseProgram
  , parseExpr
  , parseStmt
  , parseAssignment
  , parseDeclaration
  , parseConditional
  , parseParenthesizedExpr
  , parseBlock
  , parseOptionalElse
  , parseStatementsUntilBrace
  , parseWhileLoop
  , ParseError(..)  -- Ensure all constructors of ParseError are exported
  ) where


import Lexer
import AST
import Debug.Trace (traceShow)

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

-- parsePogram is the master parsing function that parses the entire program by recursively 
-- processing each statement, packaging them into the AST and returning the final result.
-- It accepts the list of tokens received from the lexer and returns a ParserResult
-- @dev Left and Right are constructors of the Either type
parseProgram :: LexedTokens -> ParserResult ([Stmt], LexedTokens)
parseProgram [] = Right ([], []) -- Base case: empty program
parseProgram tokens =            -- Recursive case: parse the program

    case parseStmt tokens of     -- call parseStmt to parse the next statement from the lexed tokens
        Left err -> Left err     -- error in parsing the statement
        Right (stmt, rest) ->    -- successfully parsed first statement

            -- Recursively parse the rest of the program
            case parseProgram rest of
                Left err -> Left err

                -- Append the parsed statement to the front of the list of stmts that have been recursively parsed
                Right (parsedStmts, remainingTokens) -> Right (stmt : parsedStmts, remainingTokens)


-------------------------------------------------------------------------------------------------- Statement Parsing

-- parseStmt accepts the lexedTokens list, determines what type of statement is next in the program, 
-- it then delegates control to the appropriate parsing function. It returns a ParserResult that contains 
-- either the parsed statement and the remaining tokens or an error.
-- @dev Stmt is a node type of the AST as defined in AST.hs
parseStmt :: LexedTokens -> ParserResult (Stmt, LexedTokens)
parseStmt [] = Left UnexpectedEndOfInput
parseStmt (t:ts) = case t of

    -- Variable declaration statements always start with a data type
    TInt -> parseDeclaration "int" ts
    TChar -> parseDeclaration "char" ts
    TDouble -> parseDeclaration "double" ts
    TFloat -> parseDeclaration "float" ts
    
    -- Assignment statements always start with a variable name.
    TIdent var -> parseAssignment var ts

    -- Parse conditional statement
    TIf -> parseConditional ts

    -- Placeholders for conditional and loop parsing
    TWhile -> parseWhileLoop ts
    
    TFor -> parseForLoopPlaceholder ts
    _ -> Left (InvalidSyntax "Invalid statement")  -- Catch-all for other patterns


-- parseBlock is a helper function that parses a block of statements enclosed in curly braces. It returns 
-- a ParserResult that contains either the parsed statements and the remaining tokens or an error.
-- @dev This function is called within parseConditional, parseWhileLoop, and parseForLoop 
parseBlock :: LexedTokens -> ParserResult ([Stmt], LexedTokens)
parseBlock tokens = case tokens of
    (TLbrace : rest) ->
        -- Parse the statements inside the block until a closing brace is encountered
        -- statements are accumulated into a list until the closing brace is found
        parseStatementsUntilBrace rest [] >>= \(stmts, TRbrace : afterBlock) ->
            
        -- Return the list of statements and the remaining tokens
        Right (stmts, afterBlock)
    _ -> Left (InvalidSyntax "Expected opening brace")

-- parseStatementsUntilBrace is a helper function that parses all 
-- statements within curly braces until a closing brace is encountered.
parseStatementsUntilBrace :: LexedTokens -> [Stmt] -> ParserResult ([Stmt], LexedTokens)
parseStatementsUntilBrace tokens acc = case tokens of

    -- If a closing brace is encountered, return the accumulated statements
    (TRbrace : _) -> Right (acc, tokens)

    -- Parse the next statement and recursively call parseStatementsUntilBrace
    _ -> parseStmt tokens >>= \(stmt, rest) ->
         parseStatementsUntilBrace rest (acc ++ [stmt])

-------------------------------------------------------------------------------------------------- Assignment Statement Delegate Function


-- parseAssignment is a delegator function called by parseStmt to parse statments determined to 
-- be assignments of preinitialized variables. It returns a ParserResult that contains either the
-- parsed statement and the remaining tokens or an error
parseAssignment :: String -> LexedTokens -> ParserResult (Stmt, LexedTokens)
parseAssignment var (TAssign : rest) = -- Assignments always start with an ideantifier followed by an assignment operator
    
    -- Parse the expression on the right-hand side of the assignment until a semicolon is encountered
    case break (== TSemicolon) rest of 
        (exprTokens, semicolon : finalTokens) ->

            -- Parse the expression and create an assignment statement node
            case parseExpr exprTokens of
                Right (expr, _) -> Right (AssignStmt var expr, finalTokens)
                Left err -> Left err

        -- If a semicolon is not found, return an error
        _ -> Left MissingSemicolon

-------------------------------------------------------------------------------------------------- Declaration Statement Delegate Function

-- parseAssignment is a delegator function called by parseStmt to parse statments determined to
-- be declarations of variables. 
parseDeclaration :: String -> LexedTokens -> ParserResult (Stmt, LexedTokens)
parseDeclaration dataType tokens = case tokens of

    -- Simple Declaration: int x;
    (TIdent var : TSemicolon : rest) ->

        -- ensure there are no additional tokens after the semicolon before
        -- packaging collected components into an ASTsimple declaration node
        if null rest
        then Right (SimpleDeclaration dataType (Var var), rest)
        else Left (UnexpectedToken (head rest))

    -- Declaration with Assignment: float f = 3.14;
    (TIdent var : TAssign : exprTokens) ->

        -- Parse the expression on the right-hand side of the assignment until a semicolon is encountered
        case break (== TSemicolon) exprTokens of
            (beforeSemi, semicolon : afterSemi) ->

                -- ensure there are no additional tokens after the semicolon
                if null afterSemi
                then case parseExpr beforeSemi of -- Parse the expression

                        -- Package the collected components into an AST declaration assignment node
                        Right (expr, []) -> Right (DeclarationAssignment dataType var expr, afterSemi)
                        Left err -> Left err
                        _ -> Left (InvalidSyntax "Invalid expression format in declaration")
                else Left (UnexpectedToken (head afterSemi))
            _ -> Left MissingSemicolon
    _ -> Left (InvalidSyntax "Invalid declaration format")


-------------------------------------------------------------------------------------------------- Conditional Statement Delegate Function

-- parseConditional is a delegator function called by parseStmt to parse statements determined to be
-- conditional statements. It returns a ParserResult that contains either the parsed statement and the
-- remaining tokens or an error.
parseConditional :: LexedTokens -> ParserResult (Stmt, LexedTokens)
parseConditional tokens = case tokens of

    (TIf : rest) ->

        -- parse conditaional statement
        parseParenthesizedExpr rest >>= \(condExpr, afterCond) ->          -- parse condition expression
        parseBlock afterCond >>= \(ifBody, afterIfBlock) ->                -- parse if's block of statements 
        parseOptionalElse afterIfBlock >>= \(elseStmt, remainingTokens) -> -- parse optional else statement

        -- Package the collected components into an AST if statement node
        Right (IfStmt condExpr ifBody elseStmt, remainingTokens)  
    _ -> Left (InvalidSyntax "Expected 'if' keyword")

-- parseOptionalElse is a helper function that parses an optional else statement. Else If statements
-- are treated as else statments that contain an If statement in their body. It returns a ParserResult 
-- that contains either the parsed statements and the remaining tokens or an error.
parseOptionalElse :: LexedTokens -> ParserResult ([Stmt], LexedTokens)
parseOptionalElse tokens = case tokens of

    -- A TIf token follows the TElse
    (TElse : TIf : rest) ->

        -- Parse the conditional and package into an AST else 
        -- statement node as the body of the if statement
        parseConditional (TIf : rest) >>= \(nestedIfStmt, afterNestedIf) ->
        Right ([ElseStmt [nestedIfStmt]], afterNestedIf)

    -- A TLbrace token follows the TElse
    (TElse : rest) ->

        -- Parse the block of statements following the else keyword
        parseBlock rest >>= \(elseBody, afterElseBlock) ->
        Right ([ElseStmt elseBody], afterElseBlock)

    -- No else found
    _ -> Right ([], tokens)

-------------------------------------------------------------------------------------------------- While Loop Statement Delegate Function

-- parseWhileLoop is a delegator function called by parseStmt to parse statements determined to be
-- while loop statements. It returns a ParserResult that contains either the parsed statement and the
-- remaining tokens or an error.
parseWhileLoop :: LexedTokens -> ParserResult (Stmt, LexedTokens)
parseWhileLoop tokens = case tokens of

    -- Parse the conditional expression
    (TWhile : rest) -> parseParenthesizedExpr rest >>= \(condExpr, afterCond) ->

        -- Parse the loop body
        parseBlock afterCond >>= \(loopBody, remainingTokens) ->
        Right (WhileStmt condExpr loopBody, remainingTokens)

    _ -> Left (InvalidSyntax "Expected 'while' keyword")


-------------------------------------------------------------------------------------------------- Generic Expression Parsing

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
    _ -> Left (InvalidSyntax "Invalid primary expression")

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

-------------------------------------------------------------------------------------------------- Placeholders for Conditional and Loop Parsing

-- Placeholder parsing function for while loops
parseWhileLoopPlaceholder :: LexedTokens -> ParserResult (Stmt, LexedTokens)
parseWhileLoopPlaceholder tokens = Left $ InvalidSyntax "While loop parsing not implemented"

-- Placeholder parsing function for for loops
parseForLoopPlaceholder :: LexedTokens -> ParserResult (Stmt, LexedTokens)
parseForLoopPlaceholder tokens = Left $ InvalidSyntax "For loop parsing not implemented"
