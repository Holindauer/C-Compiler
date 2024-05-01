-- Parser.h contains parser functions for the C subset langauge.
-- The parser is used after the Lexer, and will convert a list of tokens
-- into an abstract syntax tree (AST) that represents the program's 
-- gramatical structure.

module Parser 
  ( parseProgram
  , parseExpr
  , parseStmt
  , parseAssignment  
  ) where


import Lexer
import AST

-- Type alias for lexed tokens state
type LexedTokens = [Token]

-- parsePogram is the master parsing function that parses the 
-- entire program by recursively processing each statement
parseProgram :: LexedTokens -> ([Stmt], LexedTokens)
parseProgram [] = ([], []) 
parseProgram tokens =
    let (stmt, rest) = parseStmt tokens
    in case stmt of
        Nothing -> ([], rest)  -- If parsing fails, stop processing
        Just s  -> let (nextStmts, finalTokens) = parseProgram rest
                   in (s : nextStmts, finalTokens)


-- Determines the type of statement and delegates to the appropriate parsing function
parseStmt :: LexedTokens -> (Maybe Stmt, LexedTokens)
parseStmt [] = (Nothing, [])
parseStmt (t:ts) = case t of

    -- Variable declaration statements always start with a data type
    TInt -> parseDeclarationPlaceholder "int" ts
    TChar -> parseDeclarationPlaceholder "char" ts
    TDouble -> parseDeclarationPlaceholder "double" ts
    TFloat -> parseDeclarationPlaceholder "float" ts
    
    -- Assignment statements always start with a variable name.
    TIdent var -> parseAssignment var ts

    -- Placeholders for conditional and loop parsing
    TIf -> parseConditionalPlaceholder ts
    TWhile -> parseWhileLoopPlaceholder ts
    TFor -> parseForLoopPlaceholder ts
    _ -> (Nothing, ts)  -- Unrecognized statements or error handling

--------------------------------------------------------------------------------------------------Assignment Parsing

parseAssignment :: String -> LexedTokens -> (Maybe Stmt, LexedTokens)
parseAssignment var (TAssign : rest) =
  case break (== TSemicolon) rest of
    (exprTokens, semicolon : finalTokens) ->
      let (maybeExpr, _) = parseExpr exprTokens
      in case maybeExpr of
        Just expr -> (Just (AssignStmt var expr), finalTokens)
        Nothing -> (Nothing, rest)
    _ -> (Nothing, rest)
parseAssignment _ _ = (Nothing, [])  -- Catch-all for other patterns

--------------------------------------------------------------------------------------------------Expression Parsing

-- Parses an expression from a list of tokens
parseExpr :: LexedTokens -> (Maybe Expr, LexedTokens)
parseExpr tokens = case tokens of

    -- Parse simple assignments of literals or variables 
    (TIntLit i : rest) -> (Just (IntLit (fromIntegral i)), rest)  -- Convert Int to Integer
    (TFloatLit f : rest) -> (Just (FloatLit f), rest)
    (TDoubleLit d : rest) -> (Just (DoubleLit d), rest)
    (TIdent var : rest) -> (Just (Var var), rest)

    -- For more complex expressions, delegate to 'parseComplexExpr'
    _ -> parseComplexExpr tokens  


-- Parses complex expressions, starting with the lowest precedence
parseComplexExpr :: LexedTokens -> (Maybe Expr, LexedTokens)
parseComplexExpr tokens = parseBinaryExpr 0 tokens  -- Start with lowest precedence level

-- Set max prescedene to 6
maxPrecedence :: Int
maxPrecedence = 6

-- Parses binary expressions based on precedence using recursive descent parsing with presedence climbing
--
-- Presedence Leves for Binary Operators
-- Level 0: Logical OR (||)
-- Level 1: Logical AND (&&)
-- Level 2: Equality and inequality (==, !=)
-- Level 3: Relational (<, >, <=, >=)
-- Level 4: Addition and subtraction (+, -)
-- Level 5: Multiplication, division, and modulus (*, /, %)
-- Level 6: Unary operators (- unary, !, ~)
parseBinaryExpr :: Int -> LexedTokens -> (Maybe Expr, LexedTokens)
parseBinaryExpr precedence tokens

    -- If the precedence level is greater than the max, the expression parsed as unary
    | precedence > maxPrecedence = parseUnaryExpr tokens  
    | 
    otherwise = -- It is a binary operator and we need to parse the left and right expressions 

        -- Parse the left-hand side expression w/ a recursive call. This will increment 
        -- the precence level that will be used when parsing the right-hand side
        let (lhs, rest) = parseBinaryExpr (precedence + 1) tokens 
        in case lhs of

            -- If the left-hand side expression is not found, return nothing. This is an error case
            Nothing -> (Nothing, tokens)

            -- If something on the left found, check for an operator and parse the right-hand side
            Just left -> 

                -- Parse the operator based on the current precedence level
                let (operator, nextTokens) = parseOperator precedence rest
                in case operator of
                    Nothing -> (lhs, rest)  -- No operator found, return lhs. This is an error case
                    Just op ->
                        -- Parse the right-hand side expression w/ a recursive call
                        let (rhs, finalTokens) = parseBinaryExpr (precedence + 1) nextTokens
                        in case rhs of

                            -- If the right-hand side expression is not found, return nothing. This is an error case
                            Nothing -> (Nothing, tokens)

                            -- Now that we have the left, right, and operator, create a binary operation node
                            Just right -> (Just (BinOp op left right), finalTokens)

-- Parses an operator based on the current precedence level
-- The function returns the AST Op node and the remaining tokens
-- Parses an operator based on the current precedence level
parseOperator :: Int -> LexedTokens -> (Maybe Op, LexedTokens)
parseOperator precedence tokens = case tokens of
    (TOr : rest) | precedence == 0 -> (Just Or, rest)
    (TAnd : rest) | precedence == 1 -> (Just And, rest)
    (TEqual : rest) | precedence == 2 -> (Just Equal, rest)
    (TNotEqual : rest) | precedence == 2 -> (Just NotEqual, rest)
    (TLessThan : rest) | precedence == 3 -> (Just LessThan, rest)
    (TGreaterThan : rest) | precedence == 3 -> (Just GreaterThan, rest)
    (TLessEq : rest) | precedence == 3 -> (Just LessEq, rest)
    (TGreaterEq : rest) | precedence == 3 -> (Just GreaterEq, rest)
    (TPlus : rest) | precedence == 4 -> (Just Add, rest)
    (TMinus : rest) | precedence == 4 -> (Just Subtract, rest)
    (TStar : rest) | precedence == 5 -> (Just Multiply, rest)
    (TSlash : rest) | precedence == 5 -> (Just Divide, rest)
    (TPercent : rest) | precedence == 5 -> (Just Modulus, rest)
    _ -> (Nothing, tokens)

-- Parses unary expressions
parseUnaryExpr :: LexedTokens -> (Maybe Expr, LexedTokens)
parseUnaryExpr (TMinus : rest) =
    let (expr, finalTokens) = parsePrimaryExpr rest
    in case expr of
        Just e -> (Just (UnaryOp Neg e), finalTokens)
        Nothing -> (Nothing, rest)

parseUnaryExpr (TNot : rest) =
    let (expr, finalTokens) = parsePrimaryExpr rest
    in case expr of
        Just e -> (Just (UnaryOp LogicalNot e), finalTokens)
        Nothing -> (Nothing, rest)

parseUnaryExpr tokens = parsePrimaryExpr tokens

-- Parses primary expressions (integers, floats, variables, parenthesized expressions)
parsePrimaryExpr :: LexedTokens -> (Maybe Expr, LexedTokens)
parsePrimaryExpr (TLparen : rest) =  -- Handle parenthesized expressions
    let (expr, nextTokens) = parseExpr rest
    in case nextTokens of
        (TRparen : finalTokens) -> (expr, finalTokens) 
        _ -> (Nothing, nextTokens)
        
parsePrimaryExpr tokens = parseExpr tokens  -- Fallback to general expression parsing






-------------------------------------------------------------------------------------------------- Placeholders for Conditional and Loop Parsing

-- Parses a variable declaration statement
parseDeclarationPlaceholder :: String -> LexedTokens -> (Maybe Stmt, LexedTokens)
parseDeclarationPlaceholder dataType tokens = 
    case tokens of
        (TIdent var : TSemicolon : rest) -> (Just (Declaration dataType (Var var)), rest)
        _ -> (Nothing, tokens)  -- Handle error or incomplete declaration


-- Placeholder parsing function for conditionals
parseConditionalPlaceholder :: LexedTokens -> (Maybe Stmt, LexedTokens)
parseConditionalPlaceholder tokens = (Nothing, tokens)  -- Currently does nothing

-- Placeholder parsing function for while loops
parseWhileLoopPlaceholder :: LexedTokens -> (Maybe Stmt, LexedTokens)
parseWhileLoopPlaceholder tokens = (Nothing, tokens)  -- Currently does nothing

-- Placeholder parsing function for for loops
parseForLoopPlaceholder :: LexedTokens -> (Maybe Stmt, LexedTokens)
parseForLoopPlaceholder tokens = (Nothing, tokens)  -- Currently does nothing
