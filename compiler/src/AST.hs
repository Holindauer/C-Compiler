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
  | AssignStmt String Expr
  | Declaration String Expr
  | IfStmt Expr [Stmt] [Stmt]  -- Conditional, then-body, else-body
  | WhileStmt Expr [Stmt]
  | ForStmt Stmt Expr Stmt [Stmt] -- Initializer, condition, post-loop, body
  deriving (Eq, Show)

type Program = [Stmt]
