module AST
  ( Op(..)
  , UnaryOp(..)
  , Expr(..)
  , Stmt(..)
  , Program
  ) where

-- Binary operators
data Op = Add | Subtract | Multiply | Divide | Modulus | LessThan | GreaterThan | LessEq | GreaterEq | Equal | NotEqual | And | Or
  deriving (Eq, Show)

-- Unary operators
data UnaryOp = Neg | LogicalNot
  deriving (Eq, Show)

-- Basic expression types
data Expr
  = IntLit Integer
  | FloatLit Float
  | DoubleLit Double
  | Var String
  | BinOp Op Expr Expr
  | UnaryOp UnaryOp Expr
  deriving (Eq, Show)

-- Statements and structure of a C-like program. 
data Stmt
  = ExprStmt Expr
  | AssignStmt String Expr                   -- assignment of pre-declared variables
  | SimpleDeclaration String Expr            -- simple variable declaration: int x;
  | DeclarationAssignment String String Expr -- declaration w/ assignment: data type, variable name, initial value
  | IfStmt Expr [Stmt] [Stmt]                -- if-else statement with condition and body statements    
  | ElseStmt [Stmt]                          -- else statement with body statements (Else If Statements are Else  statements whose body is an If statement)
  | WhileStmt Expr [Stmt]                    -- while loop with condition and body statements
  | ForStmt Stmt Expr Stmt [Stmt]            -- for loop with init, condition, update, and body statements
  deriving (Eq, Show) 

type Program = [Stmt]
