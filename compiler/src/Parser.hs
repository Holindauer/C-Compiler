-- Parser.h contains parser functions for the C subset langauge.
-- The parser is used after the Lexer, and will convert a list of tokens
-- into an abstract syntax tree (AST) that represents the program's 
-- gramatical structure. The AST is defined in AST.hs.

module Parser 
  ( parseProgram
  , parseExpr
  , parseStmt
  , parseAssignment
  , ParseError(..)   -- Export the ParseError type along with its constructors
  ) where


import Lexer
import AST

-- Type alias for lexed tokens state
type LexedTokens = [Token]

-- Parser result type alias
type ParserResult a = Either ParseError a

-- This type is used to represent errors that can occur during parsing
data ParseError = UnexpectedToken Token
                | MissingSemicolon
                | InvalidSyntax String
                | UnexpectedEndOfInput
                deriving (Show, Eq)

-- parsePogram is the master parsing function that parses the 
-- entire program by recursively processing each statement
-- @dev Left and Right are constructors of the Either type
parseProgram :: LexedTokens -> ParserResult ([Stmt], LexedTokens)
parseProgram [] = Right ([], []) -- Base case: empty program
parseProgram tokens =            -- Recursive case: parse a statement and then the rest of the program
    case parseStmt tokens of
        Left err -> Left err     -- error in parsing the statement
        Right (stmt, rest) ->    -- successfully parsed a statement

            -- Recursively parse the rest of the program
            case parseProgram rest of
                Left err -> Left err
                Right (stmts, finalTokens) -> Right (stmt : stmts, finalTokens)



-- Determines the type of statement and delegates to the appropriate parsing function
-- @dev Stmt is a node type of the AST as defined in AST.hs
parseStmt :: LexedTokens -> ParserResult (Stmt, LexedTokens)
parseStmt [] = Left UnexpectedEndOfInput
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
    _ -> Left (InvalidSyntax "Invalid statement")  -- Catch-all for other patterns

--------------------------------------------------------------------------------------------------Assignment Parsing

-- Parses an assignment statement
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



--------------------------------------------------------------------------------------------------Expression Parsing

-- Parses an expression from a list of tokens
parseExpr :: LexedTokens -> ParserResult (Expr, LexedTokens)
parseExpr tokens = case tokens of

    -- If primary expression, parse it directly
    (TIntLit i : rest) -> Right (IntLit (fromIntegral i), rest)  
    (TFloatLit f : rest) -> Right (FloatLit f, rest)           
    (TDoubleLit d : rest) -> Right (DoubleLit d, rest)           
    (TIdent var : rest) -> Right (Var var, rest)                

    -- If it's not a simple expression, delegate to parseComplexExpr
    _ -> parseComplexExpr tokens

-- Parses more complex expressions, starting with the lowest precedence
parseComplexExpr :: LexedTokens -> ParserResult (Expr, LexedTokens)
parseComplexExpr tokens = parseBinaryExpr 0 tokens -- Start with the lowest precedence level

-- Set max prescedene to 6 for unary operators
maxPrecedence :: Int
maxPrecedence = 6

-- Parses binary expressions based on precedence using recursive descent parsing with presedence climbing
--
-- Presedence Levels for Binary Operators
-- Level 0: Logical OR (||)
-- Level 1: Logical AND (&&)
-- Level 2: Equality and inequality (==, !=)
-- Level 3: Relational (<, >, <=, >=)
-- Level 4: Addition and subtraction (+, -)
-- Level 5: Multiplication, division, and modulus (*, /, %)
-- Level 6: Unary operators (- unary, !, ~)
parseBinaryExpr :: Int -> LexedTokens -> ParserResult (Expr, LexedTokens)
parseBinaryExpr precedence tokens

    -- If the precedence level is greater than the max, parse as a unary expression
    | precedence > maxPrecedence = parseUnaryExpr tokens

    -- otherwise expression is binary. Recursively parse the left and right sides
    -- Note that we are increasing the precedence level by 1 for the left side
    | otherwise = case parseBinaryExpr (precedence + 1) tokens of 
        
        -- Parse the left-hand side of the expression
        Left err -> Left err
        Right (lhs, rest) -> case parseOperator precedence rest of

            -- Parse the operator and the right-hand side of the expression
            Left err -> Left err
            Right (op, nextTokens) -> case parseBinaryExpr (precedence + 1) nextTokens of

                -- Parse the right-hand side of the expression and create a binary operation node
                Left err -> Left err
                Right (rhs, finalTokens) -> Right (BinOp op lhs rhs, finalTokens)

-- Parses an operator based on the current precedence level
-- The function returns the AST Op node and the remaining tokens
-- Parses an operator based on the current precedence level
parseOperator :: Int -> LexedTokens -> ParserResult (Op, LexedTokens)
parseOperator precedence tokens = case tokens of
    (TOr : rest) | precedence == 0 -> Right (Or, rest)
    (TAnd : rest) | precedence == 1 -> Right (And, rest)
    (TEqual : rest) | precedence == 2 -> Right (Equal, rest)
    (TNotEqual : rest) | precedence == 2 -> Right (NotEqual, rest)
    (TLessThan : rest) | precedence == 3 -> Right (LessThan, rest)
    (TGreaterThan : rest) | precedence == 3 -> Right (GreaterThan, rest)
    (TLessEq : rest) | precedence == 3 -> Right (LessEq, rest)
    (TGreaterEq : rest) | precedence == 3 -> Right (GreaterEq, rest)
    (TPlus : rest) | precedence == 4 -> Right (Add, rest)
    (TMinus : rest) | precedence == 4 -> Right (Subtract, rest)
    (TStar : rest) | precedence == 5 -> Right (Multiply, rest)
    (TSlash : rest) | precedence == 5 -> Right (Divide, rest)
    (TPercent : rest) | precedence == 5 -> Right (Modulus, rest)
    _ -> Left $ UnexpectedToken (head tokens)


-- Parses unary expressions
parseUnaryExpr :: LexedTokens -> ParserResult (Expr, LexedTokens)
parseUnaryExpr (TMinus : rest) = -- Parse unary expressions for negation and logical NOT
    parsePrimaryExpr rest >>= \(expr, finalTokens) -> Right (UnaryOp Neg expr, finalTokens)
parseUnaryExpr (TNot : rest) = -- Parse unary expressions for logical NOT
    parsePrimaryExpr rest >>= \(expr, finalTokens) -> Right (UnaryOp LogicalNot expr, finalTokens)

-- If no unary operator is found, parse the primary expression
parseUnaryExpr tokens = parsePrimaryExpr tokens




-- Parses primary expressions (integers, floats, variables, parenthesized expressions)
parsePrimaryExpr :: LexedTokens -> ParserResult (Expr, LexedTokens)
parsePrimaryExpr (TLparen : rest) = 
    parseExpr rest >>= \(expr, nextTokens) -> case nextTokens of
        (TRparen : finalTokens) -> Right (expr, finalTokens)
        _ -> Left MissingSemicolon  -- More specific error if needed
parsePrimaryExpr tokens = Left $ InvalidSyntax "Invalid primary expression"



-------------------------------------------------------------------------------------------------- Placeholders for Conditional and Loop Parsing

-- Parses a variable declaration statement
parseDeclarationPlaceholder :: String -> LexedTokens -> ParserResult (Stmt, LexedTokens)
parseDeclarationPlaceholder dataType tokens = 
    case tokens of
        (TIdent var : TSemicolon : rest) -> Right (Declaration dataType (Var var), rest)
        _ -> Left MissingSemicolon


-- Placeholder parsing function for conditionals
parseConditionalPlaceholder :: LexedTokens -> ParserResult (Stmt, LexedTokens)
parseConditionalPlaceholder tokens = Left $ InvalidSyntax "Conditional parsing not implemented"

-- Placeholder parsing function for while loops
parseWhileLoopPlaceholder :: LexedTokens -> ParserResult (Stmt, LexedTokens)
parseWhileLoopPlaceholder tokens = Left $ InvalidSyntax "While loop parsing not implemented"

-- Placeholder parsing function for for loops
parseForLoopPlaceholder :: LexedTokens -> ParserResult (Stmt, LexedTokens)
parseForLoopPlaceholder tokens = Left $ InvalidSyntax "For loop parsing not implemented"
