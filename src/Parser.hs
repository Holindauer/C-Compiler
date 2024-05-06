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
  , parseForLoop
  , extractForLoopHeader    
  , parseForLoopHeader      
  , parseUpdateStatement    
  , ensureSemicolon         
  , ParseError(..)  -- Ensure all constructors of ParseError are exported
  ) where


import Lexer
import AST
import Debug.Trace (traceShow, trace)

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
parseStmt (t:ts) = trace ("parseStmt: Processing " ++ show t ++ " with remaining " ++ show ts) $ case t of

    -- Variable declaration statements always start with a data type
    TInt -> trace "parseStmt: Parsing int declaration" $ parseDeclaration "int" ts
    TChar -> trace "parseStmt: Parsing char declaration" $ parseDeclaration "char" ts
    TDouble -> trace "parseStmt: Parsing double declaration" $ parseDeclaration "double" ts
    TFloat -> trace "parseStmt: Parsing float declaration" $ parseDeclaration "float" ts
    
    -- Assignment statements always start with a variable name.
    TIdent var -> trace ("parseStmt: Parsing assignment for " ++ var) $ parseAssignment var ts

    -- Parse conditional statement
    TIf -> trace "parseStmt: Parsing if statement" $ parseConditional ts

    -- Parse loop statements
    TWhile -> trace "parseStmt: Parsing while loop" $ parseWhileLoop ts
    TFor -> trace "parseStmt: Parsing for loop" $ parseForLoop ts

    _ -> Left (InvalidSyntax $ "Invalid statement at token: " ++ show t)




-- parseBlock is a helper function that parses a block of multiple statements enclosed in 
-- curly braces. It returns a ParserResult that contains either the parsed statements and 
-- the remaining tokens or an error.
-- @dev This function is called within parseConditional, parseWhileLoop, and parseForLoop 
parseBlock :: LexedTokens -> ParserResult ([Stmt], LexedTokens)
parseBlock tokens = case tokens of

    -- curly brace block
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

-- -- parseAssignment is a delegator function called by parseStmt to parse statments determined to 
-- -- be assignments of preinitialized variables. It returns a ParserResult that contains either the
-- -- parsed statement and the remaining tokens or an error
parseAssignment :: String -> LexedTokens -> ParserResult (Stmt, LexedTokens)
parseAssignment var (TAssign : rest) = 
    case break (== TSemicolon) rest of -- Break the tokens at the semicolon
 
        -- Expression followed by a semicolon
        (exprTokens, semicolon : finalTokens) -> 

            case parseExpr exprTokens of
                Right (expr, _) -> Right (AssignStmt var expr, finalTokens)
                Left err -> Left err
        
        -- Handle edge cases (empty assignment, missing semicolon, etc.)
        ([], []) -> Left (InvalidSyntax "Assignment without expression")
        (_, []) -> Left MissingSemicolon 
        _ -> Left (InvalidSyntax "Malformed assignment")

-- Handle invalid assignment statements        
parseAssignment _ [] = Left (InvalidSyntax "Unexpected end of input in assignment")
parseAssignment _ _ = Left (InvalidSyntax "Malformed assignment statement")

-------------------------------------------------------------------------------------------------- Declaration Statement Delegate Function

-- parseAssignment is a delegator function called by parseStmt to parse statments determined to
-- be declarations of variables. 
parseDeclaration :: String -> LexedTokens -> ParserResult (Stmt, LexedTokens)
parseDeclaration dataType tokens = case tokens of
    -- Simple Declaration: int x;
    (TIdent var : TSemicolon : rest) ->
        Right (SimpleDeclaration dataType (Var var), rest)

    -- Declaration with Assignment: float f = 3.14;
    (TIdent var : TAssign : exprTokens) ->
        let (beforeSemi, afterSemi) = break (== TSemicolon) exprTokens
        in if not (null afterSemi) then
            case parseExpr beforeSemi of
                Right (expr, _) -> Right (DeclarationAssignment dataType var expr, tail afterSemi)
                Left err -> Left err
           else
               Left (InvalidSyntax "parseDeclaration: Missing semicolon in declaration")

    _ -> Left (InvalidSyntax "parseDeclaration: Invalid declaration format")


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


-------------------------------------------------------------------------------------------------- For Loop Statement Delegate Function


-- parseForLoop is a delegator function called by parseStmt to parse statements determined to be
-- for loop statements. It returns a ParserResult that contains either the parsed statement and the
-- remaining tokens or an error.
parseForLoop :: LexedTokens -> ParserResult (Stmt, LexedTokens)
parseForLoop (TFor : rest) = trace "Starting parseForLoop" $  -- TFor token not includer in rest

    -- Extract the tokens inside the parentheses of the for loop header
    -- rest should contain [TLparen, .... , TRparen, TLbrace, ..., TRbrace]
    extractForLoopHeader rest >>= \(headerTokens, afterHeader) ->
    trace ("Header tokens: " ++ show headerTokens) (
        
        -- Parse the three components of the for loop header. Note that headerTokens should 
        -- now contain the tokens inside the parentheses but not including them.
        parseForLoopHeader headerTokens >>= \(initStmt, condExpr, updateStmt) ->

        -- Parse the loop body. Note that the after header tokens should now contain
        -- the loop body enclosed in curly braces.
        parseBlock afterHeader >>= \(loopBody, remainingTokens) ->

        -- Package the parsed components into an AST for statement node
        Right (ForStmt initStmt condExpr updateStmt loopBody, remainingTokens))
parseForLoop _ = Left (InvalidSyntax "parseForLoop: Expected 'for' followed by '('")


-- Extracts tokens inside the parentheses of the for loop header
-- Input tokens list expected to start with [TLparen, .... , TRparen, TLbrace, ..., TRbrace]
extractForLoopHeader :: LexedTokens -> ParserResult ([Token], LexedTokens)
extractForLoopHeader tokens =
    case tokens of
        (TLparen : rest) ->  -- Confirm the list starts with TLparen and process the rest
            let (headerTokens, afterHeader) = break (== TRparen) rest
            in if null afterHeader
               then Left (InvalidSyntax "Expected closing parenthesis for for loop header")
               else -- Successfully found TRparen, skip it using tail
                    -- Also ensure there is a token following the closing parenthesis to handle correctly
                    traceShow ("Header tokens inside parentheses:", headerTokens) $
                    traceShow ("Remaining tokens after header:", tail afterHeader) $
                    Right (headerTokens, tail afterHeader)
        _ -> Left (InvalidSyntax "extractForLoopHeader: Expected opening parenthesis at the start of for loop header")


-- Parses the three components of a for loop header
-- For loop header always expect to contain an int declaration init statement, a 
-- conditional expression, and an update statement.
parseForLoopHeader :: LexedTokens -> ParserResult (Stmt, Expr, Stmt)
parseForLoopHeader (TInt: rest) = do

    -- parse the initialization statement
    (initStmt, afterInit) <- parseDeclaration "int" rest

    -- parse the conditional expression
    (condExpr, afterCond) <- parseExpr afterInit >>= \(expr, remainingTokens) ->

        -- Ensure a semicolon follows the conditional expression
        case remainingTokens of
            (TSemicolon : rest) -> Right (expr, rest)
            _ -> Left (InvalidSyntax "parseForLoopHeader: Expected semicolon after conditional expression")

    -- parse the update statement   
    (updateStmt, afterUpdate) <- parseUpdateStatement afterCond

    -- Package the parsed components into an AST for statement node
    if null afterUpdate
    then Right (initStmt, condExpr, updateStmt)
    else Left (InvalidSyntax "parseForLoopHeader: Extra tokens in for loop header")
parseForLoopHeader _ =  Left (InvalidSyntax "parseForLoopHeader: Invalid for loop header")



    

-- Helper to ensure and consume a semicolon, returning the rest of the tokens
ensureSemicolon :: LexedTokens -> ParserResult LexedTokens
ensureSemicolon (TSemicolon : rest) = Right rest
ensureSemicolon _ = Left (InvalidSyntax "Expected semicolon")

-- Parses increment or simple assignment as update statements in the for loop
parseUpdateStatement :: LexedTokens -> ParserResult (Stmt, LexedTokens)
parseUpdateStatement tokens = case tokens of
    -- Post increment 
    (TIdent var : TIncrement : rest) -> 
        if null rest 
        then Right (IncrementStmt var, rest)
        else Left (InvalidSyntax "parseUpdateStatement: No additional tokens expected after increment")

    -- Post decrement
    (TIdent var : TDecrement : rest) ->
        if null rest 
        then Right (DecrementStmt var, rest)
        else Left (InvalidSyntax "parseUpdateStatement: No additional tokens expected after decrement")

    -- Pre increment 
    (TIncrement : TIdent var : rest) ->
        if null rest 
        then Right (IncrementStmt var, rest)
        else Left (InvalidSyntax "parseUpdateStatement: No additional tokens expected after increment")
    
    -- Pre decrement
    (TDecrement : TIdent var : rest) ->
        if null rest 
        then Right (DecrementStmt var, rest)
        else Left (InvalidSyntax "parseUpdateStatement: No additional tokens expected after decrement")

    -- Unexpected token handling
    (token : _) -> trace ("Unexpected token in update statement: " ++ show token) $
         Left (UnexpectedToken token)
    _ -> Left (InvalidSyntax "parseUpdateStatement: Invalid or missing update statement in for loop header")


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

