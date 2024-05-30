module CodeGen_Helper where

import AST
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Hashable



-- type alias for a hashmap of variable names and their types
type TypeMap = HashMap String VarType

-- uncurry3 is a modified version of the uncurry built-in for packaging data into a tuple that works with 3-tuples
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z


-- func to gen instruction for moving value in output register into a variable for a specific expr type 
moveOutputIntoVarInstr :: VarType -> String -> String
moveOutputIntoVarInstr exprType lValue = case exprType of
  IntType -> "\tmov [" ++ lValue ++ "_label], rax\n"       -- int and char use rax
  CharType -> "\tmov [" ++ lValue ++ "_label], rax\n"  
  FloatType -> "\tmovss [" ++ lValue ++ "_label], xmm0\n"  -- float uses xmm0
  DoubleType -> "\tmovsd [" ++ lValue ++ "_label], xmm1\n" -- double uses xmm1
  _ -> error "Unsupported expression type"

-- func to gen instruction for moving a literal into a type specific output register
moveLitIntoOutRegInstr :: VarType -> String -> String
moveLitIntoOutRegInstr exprType value = case exprType of
  IntType -> "\tmov rax, " ++ value ++ "\n"       -- int and char use rax
  CharType -> "\tmov rax, " ++ value ++ "\n"  
  FloatType -> "\tmovss xmm0, " ++ value ++ "\n"  -- float uses xmm0
  DoubleType -> "\tmovsd xmm1, " ++ value ++ "\n" -- double uses xmm1
  _ -> error "Unsupported expression type"

-- func to determine the type of an expression. Literal types are determined by the which type of literal statement 
-- they belong to. Variables are searched within the provided TypeMap. Unary and binary opererations are determined 
-- by a depth first search for either the first literal or variable in the expression AST. Syntax is assumed to be
-- correct, so the function does not expect multiple types in a single expression.
getExprType :: Expr -> TypeMap -> VarType 
getExprType expr typeMap = case expr of  

  -- literals
  IntLit _ -> IntType
  FloatLit _ -> FloatType
  DoubleLit _ -> DoubleType
  CharLit _ -> CharType

  -- variables
  Var name -> case HashMap.lookup name typeMap of
    Just varType -> varType
    Nothing -> error "Variable not found in type map"

  -- unary and binary ops
  UnaryOp _ subExpr -> getExprType subExpr typeMap
  BinOp _ lhs _ -> getExprType lhs typeMap -- only lhs need be checked
  
  _ -> error "Unsupported expression type"