module Helper where

import AST


-- uncurry3 is a modified version of the uncurry built-in for packaging data into a tuple that works with 3-tuples
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z


-- func to gen instruction for moving value in output register into a variable for a specific expr type 
moveOutputIntoVarInstr :: String -> String -> String
moveOutputIntoVarInstr exprType lValue = case exprType of
  "Int" -> "\tmov [" ++ lValue ++ "_label], rax\n"       -- int and char use rax
  "Char" -> "\tmov [" ++ lValue ++ "_label], rax\n"  
  "Float" -> "\tmovss [" ++ lValue ++ "_label], xmm0\n"  -- float uses xmm0
  "Double" -> "\tmovsd [" ++ lValue ++ "_label], xmm1\n" -- double uses xmm1
  _ -> error "Unsupported expression type"

-- func to gen instruction for moving a literal into a type specific output register
moveLitIntoOutRegInstr :: String -> String -> String
moveLitIntoOutRegInstr exprType value = case exprType of
  "Int" -> "\tmov rax, " ++ value ++ "\n"       -- int and char use rax
  "Char" -> "\tmov rax, " ++ value ++ "\n"  
  "Float" -> "\tmovss xmm0, " ++ value ++ "\n"  -- float uses xmm0
  "Double" -> "\tmovsd xmm1, " ++ value ++ "\n" -- double uses xmm1
  _ -> error "Unsupported expression type"

-- func to determine the type of an expression  
getExprType :: Expr -> String 
getExprType expr = case expr of  -- ! Currently Placeholder

  -- literals
  IntLit _ -> "Int"
  FloatLit _ -> "Float"
  DoubleLit _ -> "Double"
  CharLit _ -> "Char"

  -- variables TODO implement
  


  _ -> error "Unsupported expression type"