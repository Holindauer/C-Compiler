{-# LANGUAGE RankNTypes #-}


module CodeGen_Expressions where

import AST
import Data.List (foldl', zipWith)
import Debug.Trace (traceShow, trace)

-------------------------------------------------------------------------------------------------- Expression Evaluation Subroutine generation

-- genExprSr recursively chains subroutine definitions together for evaluating expressions in a depth first manner. 
-- Literal/Variable expressions are the base case. Unary/Binary expressions will result in recursively chaining of
-- subroutines to handle the evaluation of the subexpressions.
-- @param The base name of expression evaluation subroutine, the number of subroutines that have already been chained 
-- from the base to get to this one, and the current expr/subexpr.
genExprEvalSr :: String -> Integer -> Expr -> (String, String, Integer)
genExprEvalSr baseName index expr = case expr of

  -- Base Cases: expr is a literal or variable
  IntLit value ->
    literalEvalSr baseName index value "Int"
  FloatLit value ->
    literalEvalSr baseName index value "Float"
  DoubleLit value ->
    literalEvalSr baseName index value "Double"
  CharLit value ->
    literalEvalSr baseName index value "Char"
  Var varName -> 
    variableEvalSr baseName index varName

  -- Recursive Cases: expr is a unary or binary operation 
  UnaryOp op subExpr ->
    unaryOpEvalSr baseName index op subExpr
  BinOp op lhs rhs ->
    binaryOpEvalSr baseName index op lhs rhs
    
  _ -> error "Unsupported expression type"


-- Helper func to generate subroutine that moves a literal value into rax during expressions eval 
literalEvalSr :: String -> Integer -> Show a => a -> String -> (String, String, Integer)
literalEvalSr baseName index value typeName =
  let
    -- set unique subroutine name
    subroutineName = baseName ++ "_" ++ show index

    -- set subroutine definition
    subroutineDef = subroutineName ++ ":\n" ++
                    "\tmov rax, " ++ show value ++ "\n" ++
                    "\tret\n"
  in (subroutineDef, subroutineName, index) -- return subroutine def, name, updated idx


-- Helper func to generate subroutnie that moves the value of a variable into rax during expression eval
variableEvalSr :: String -> Integer -> String -> (String, String, Integer)
variableEvalSr baseName index varName =
  let
      -- set unique subroutine name derived from base name, varName, and index
      subroutineName = baseName ++ "_" ++ show index

      -- set subroutine definition 
      subroutineDef = subroutineName ++ ":\n" ++                   -- subroutine label
                      "\tmov rax, [" ++ varName ++ "_label]\n" ++  -- move value of varName into rax
                      "\tret\n"                                    -- return from subroutine

    in (subroutineDef, subroutineName, index) -- return the subroutine def, name, updated idx

-- Helper func to generate subroutine that evaluates a unary operation recursively by chaining
-- subroutine definitions for evaluating all subexpressions together. The output of the subexpression 
-- is stored in the rax register the unary operation is applied.
unaryOpEvalSr :: String -> Integer -> UnaryOp -> Expr -> (String, String, Integer)
unaryOpEvalSr baseName index op subExpr =
  let
      -- recursively evaluate the subexpression. NOTE that the index passed in is incremented
      (subExprDef, subExprName, newIndex) = genExprEvalSr baseName (index + 1) subExpr

      -- set unique subroutine name derived from base name, unary op, and index
      subroutineName = baseName ++  "_" ++ show index

      -- set subroutine definition 
      subroutineDef = subroutineName ++ ":\n" ++           -- subroutine label
                      "\tcall " ++ subExprName ++ "\n" ++  -- call the subexpression eval subroutine
                      unaryOpAsm op ++                     -- use unaryOpAsm to generate instructions for specific op
                      "\tret\n"                            -- return from subroutine

      -- append the subExprDef to the subroutine definition
      fullSubroutineDef = subroutineDef ++ subExprDef

    -- return the full subroutine def, name, updated idx
    in (fullSubroutineDef, subroutineName, newIndex)

-- Helper func to generate a subroutine that evaluates a binary operation recursively by 
-- generating a chain of subroutine definitions that will evaluate the left and right hand
-- subexpressions in a depth first manner. The output of the left hand side expression is
-- stored on the stack before evaluating the rhs. The final result is stored in the rax register.
binaryOpEvalSr :: String -> Integer -> Op -> Expr -> Expr -> (String, String, Integer)
binaryOpEvalSr baseName index op lhs rhs =
  let

      -- Generate subroutine definitions for evaluating LHS and RHS
      (lhsExprEvalSrDef, lhsExprEvalSrName, lhsLastIndex) = genExprEvalSr (baseName ++ "_lhs_eval") (index) lhs
      (rhsExprEvalSrDef, rhsExprEvalSrName, rhsLastIndex) = genExprEvalSr (baseName ++ "_rhs_eval") (lhsLastIndex) rhs

      -- Unique subroutine name for the binary operation
      binaryOpSrName = baseName ++ "_" ++ show index

      -- Define the binary operation subroutine
      binaryOpSrDef = binaryOpSrName ++ ":\n" ++

          -- Evaluate the LHS expression and store the result temporarily
          "\tcall " ++ lhsExprEvalSrName ++ "\n" ++
          "\tpush rax\n" ++  -- Store LHS result on the stack

          -- Evaluate the RHS expression
          "\tcall " ++ rhsExprEvalSrName ++ "\n" ++
          "\tmov rbx, rax\n" ++  -- Move RHS result to rbx
          "\tpop rax\n" ++  -- Retrieve LHS result from stack to rax

          -- generate the binary operation instructions using binaryOpAsm
          binaryOpAsm op ++ 
          "\tret\n"

      -- Combine the subroutine definitions with the binary operation code
      fullBinaryOpSrDef = lhsExprEvalSrDef ++ rhsExprEvalSrDef ++ binaryOpSrDef

    -- Return the complete binary operation sr def, name, updated idx
    in (fullBinaryOpSrDef, binaryOpSrName, rhsLastIndex)


-- unaryOpAsm is a helper function called within genExprEvalSr that generates the 
-- NASM assembly code for a specific unary operation based on the provided unary op.
-- It is assumed that the value for which the operation is being applied is already
-- within the rax register.
unaryOpAsm :: UnaryOp -> String
unaryOpAsm op = case op of
  Neg -> "\tneg rax\n"
  LogicalNot -> "\tnot rax\n"
  Increment -> "\tinc rax\n"
  Decrement -> "\tdec rax\n"
  _ -> error "Operation not supported"

-- binaryOpAsm is a helper function called within genExprEvalSr that generates the
-- NASM assembly code for a specific binary operation based on the provided binary op.s
-- It is assumed that the values for which the operation is being applied are already
-- within the rax and rbx registers. Commutativity does not apply to this setup.
binaryOpAsm :: Op -> String
binaryOpAsm op = case op of

  -- Arithmetic operations
  Add -> "\tadd rax, rbx\n"
  Subtract -> "\tsub rax, rbx\n"
  Multiply -> "\timul rax, rbx\n"
  Divide -> "\tcqo\n" ++ "\tidiv rbx\n"  -- Sign-extend rax into rdx:rax before division

  -- Comparison operations
  LessThan -> "\tcmp rax, rbx\n" ++ "\tsetl al\n" ++ "\tmovzx rax, al\n"
  GreaterThan -> "\tcmp rax, rbx\n" ++ "\tsetg al\n" ++ "\tmovzx rax, al\n"
  LessEq -> "\tcmp rax, rbx\n" ++ "\tsetle al\n" ++ "\tmovzx rax, al\n"
  GreaterEq -> "\tcmp rax, rbx\n" ++ "\tsetge al\n" ++ "\tmovzx rax, al\n"
  Equal -> "\tcmp rax, rbx\n" ++ "\tsete al\n" ++ "\tmovzx rax, al\n"
  NotEqual -> "\tcmp rax, rbx\n" ++ "\tsetne al\n" ++ "\tmovzx rax, al\n"

  -- Logical operations
  And -> "\tand rax, rbx\n" ++ "\ttest rax, rax\n" ++ "\tsetnz al\n" ++ "\tmovzx rax, al\n"
  Or -> "\tor rax, rbx\n" ++ "\ttest rax, rax\n" ++ "\tsetnz al\n" ++ "\tmovzx rax, al\n"

  _ -> error "Operation not supported"
