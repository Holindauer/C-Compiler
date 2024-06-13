module CodeGen_Expressions where

import AST
import CodeGen_Declarations 
import CodeGen_Helper
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Hashable


-------------------------------------------------------------------------------------------------- Expression Evaluation Subroutine generation

-- genExprSr recursively generates and chains together subroutine definitions for evaluating an expression. 
-- This involves a depth first traversal of the AST containing the expr. Literal/Variable expr are the base 
-- case. Unary/Binary expr recurse.
-- Recursively generates and chains together subroutine definitions for evaluating an expression
genExprEvalSr :: String -> Integer -> Expr -> TypeMap -> FloatMap -> (String, String, Integer)
genExprEvalSr baseName idx expr typeMap floatMap = case expr of

  -- literal base case
  IntLit value -> literalEvalSr baseName idx (show value) IntType floatMap
  FloatLit value -> literalEvalSr baseName idx (show value) FloatType floatMap
  DoubleLit value -> literalEvalSr baseName idx (show value) DoubleType floatMap
  CharLit value -> literalEvalSr baseName idx (show (fromEnum value)) CharType floatMap

  -- variable base case
  Var varName -> variableEvalSr baseName idx varName (typeMap HashMap.! varName) floatMap

  -- recursive cases (binary and unary ops)
  UnaryOp op subExpr ->
    let (subSr, subName, newIdx) = genExprEvalSr baseName idx subExpr typeMap floatMap
    in unaryOpEvalSr baseName newIdx op subExpr typeMap floatMap



  -- BinOp op lhs rhs ->
  --   let (lhsSr, lhsName, newIndex) = genExprEvalSr baseName index lhs typeMap
  --       (rhsSr, rhsName, finalIndex) = genExprEvalSr baseName newIndex rhs typeMap
  --   in binaryOpEvalSr baseName finalIndex op lhsName rhsName

  _ -> error "Unsupported expression type"

-- generates sr for moving lieral into output register
literalEvalSr :: String -> Integer -> String -> DataType -> FloatMap -> (String, String, Integer)
literalEvalSr baseName idx value valueType floatMap =
  let
    srName = baseName ++ "_" ++ show idx                           -- sr name
    moveInstruction = moveInstr_LitToReg valueType value floatMap  -- move instr
    srDef = srName ++ ":\n" ++ moveInstruction ++ "\tret\n"        -- sr def

  in (srDef, srName, idx + 1) -- inc idx for next sr inc case of nesting

-- generates sr for moving value within a variable into an output register
variableEvalSr :: String -> Integer -> String -> DataType -> FloatMap -> (String, String, Integer)
variableEvalSr baseName idx varName dataType floatMap =
  let
    srName = baseName ++ "_" ++ show idx                    -- sr name 
    moveInstruction = moveInstr_VarToReg dataType varName   -- move instr
    srDef = srName ++ ":\n" ++ moveInstruction ++ "\tret\n" -- sr def

  in (srDef, srName, idx + 1) -- inc idx for next sr inc case of nesting

-------------------------------------------------------------------------------------------------- Unary Op Eval Subroutine Generation

-- Generates subroutine that evaluates a unary operation recursively by first creating a chain of subroutines that 
-- eval any nested expressions in a depth first manner. Then the (type specific) unary op is applied to the result
-- that was stored in the output register.
unaryOpEvalSr :: String -> Integer -> UnaryOp -> Expr-> TypeMap -> FloatMap-> (String, String, Integer)
unaryOpEvalSr baseName index op subExpr typeMap floatMap =
  let
      -- recursively evaluate the subexpression. NOTE the idx is incremented
      (subExprDef, subExprName, newIndex) = genExprEvalSr baseName (index + 1) subExpr typeMap floatMap

      -- set unique subroutine name derived from base name, unary op, and index
      subroutineName = baseName ++  "_" ++ show index

      -- set subroutine definition 
      subroutineDef = subroutineName ++ ":\n" ++                                -- subroutine label
                      "\tcall " ++ subExprName ++ "\n" ++                       -- call the subexpression eval subroutine
                      typeSpecific_UnaryOp op (getExprType subExpr typeMap) ++  -- use unaryOpAsm to generate instructions for specific op
                      "\tret\n"                                                 -- return from subroutine

      -- append the subExprDef to the subroutine definition
      fullSubroutineDef = subroutineDef ++ subExprDef

    -- return the full subroutine def, name, updated idx
    in (fullSubroutineDef, subroutineName, newIndex)


-- typeSpecific_UnaryOp is a helper function called within genExprEvalSr that generates the 
-- NASM assembly code for a specific unary operation based on the provided unary op.
-- It is assumed that the value for which the operation is being applied is already
-- within the rax/xmm0/xmm1 register.
typeSpecific_UnaryOp :: UnaryOp -> DataType -> String
typeSpecific_UnaryOp op exprType = case op of

  Neg -> case exprType of
    IntType -> "\tneg rax\n"
    CharType -> "\tneg rax\n"
    FloatType -> "\tnegss xmm0, xmm0\n"
    DoubleType -> "\tnegsd xmm1, xmm1\n"
    
  LogicalNot -> case exprType of 
    IntType -> "\tnot rax\n"
    CharType -> "\tnot rax\n"
    -- FloatType -> ""  -- ! determine what to do about these? boolean expressions arent only implemented in the context of
    -- DoubleType -> "" -- ! conditionals so this might require extra infrastructure.
    _ -> error "Operation not supported"

  Increment -> case exprType of 
    IntType -> "\tinc rax\n"
    CharType -> "\tinc rax\n"
    FloatType -> "\taddss xmm0, [one_float]\n" -- one_float, one_double defined in .data section
    DoubleType -> "\taddsd xmm1, [one_double]\n"

  Decrement -> case exprType of
    IntType -> "\tdec rax\n"
    CharType -> "\tdec rax\n"
    FloatType -> "\tcall decrement_float\n" -- helper sr defined in CodeGen_Main.hs
    DoubleType -> "\tcall decrement_double\n" -- helper sr defined in CodeGen_Main.hs

  _ -> error "Operation not supported"