module AST where

-- AST.hs contains the definition of the abstract syntax tree (AST) for the C-like language.
-- A statement is considered either a single line of code punctuated by a semicolon or a block
-- of code w/ associated header and enclosed in curly braces (for loop, while loopm conditional),
-- Each statement within a source file is combined into a list of stmts, which is the Program type.
-- A stmt is a type that contains the gramatical structure of the statement, including all nested 
-- statements and expressions. 

-- Binary operators
data Op = Add | Subtract | Multiply | Divide | Modulus
        | LessThan | GreaterThan | LessEq | GreaterEq | Equal | NotEqual
        | And | Or
        | PlusAssign | MinusAssign | MultAssign | DivAssign | ModAssign  -- Compound assignment operators
  deriving (Eq, Show)

-- Unary operators
data UnaryOp = Neg | LogicalNot | Increment | Decrement
  deriving (Eq, Show)

-- Basic expression types
data Expr
  = IntLit Integer
  | FloatLit Float
  | DoubleLit Double
  | CharLit Char  
  | Var String
  | BinOp Op Expr Expr
  | UnaryOp UnaryOp Expr
  deriving (Eq, Show)

-- Statements and structure of a C-like program
data Stmt
  = ExprStmt Expr
  | AssignStmt String Expr                 -- assignment of pre-declared variables
  | CompoundAssignStmt String Op Expr      -- compound assignment: variable, operation, and expression
  | IncrementStmt String                   -- increment statement: variable name
  | DecrementStmt String                   -- decrement statement: variable name
  | SimpleDeclaration String Expr          -- simple variable declaration: int x;
  | DeclarationAssignment String String Expr  -- declaration with assignment: data type, variable name, initial value
  | IfStmt Expr [Stmt] [Stmt]              -- if-else statement with [conditional expr, body statements, else body statements]    
  | ElseStmt [Stmt]                        -- else statement with body statements (Else If Statements are Else statements whose body is an If statement)
  | WhileStmt Expr [Stmt]                  -- while loop with condition and body statements
  | ForStmt Stmt Expr Stmt [Stmt]          -- for loop with init, condition, update, and body statements
  | ReturnStmt Expr                        -- return statement with expression
  deriving (Eq, Show)

type Program = [Stmt]
