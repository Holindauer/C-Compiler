module CodeGen_Expressions where

import AST
import CodeGen_Declarations 
import CodeGen_Helper
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)


-------------------------------------------------------------------------------------------------- Expression Evaluation Subroutine generation

-- genExprSr recursively generates and chains together subroutine definitions for evaluating an expression. 
-- This involves a depth first traversal of the AST containing the expr. Literal/Variable expr are the base 
-- case. Unary/Binary expr recurse.
-- Recursively generates and chains together subroutine definitions for evaluating an expression
genExprEvalSr :: String -> Integer -> Expr -> TypeMap -> (String, String, Integer)
genExprEvalSr baseName idx expr typeMap = case expr of

  -- base cases, var and lit
  IntLit value ->
    literalEvalSr baseName idx (show value) IntType
  FloatLit value ->
    literalEvalSr baseName idx (show value) FloatType
  DoubleLit value ->
    literalEvalSr baseName idx (show value) DoubleType
  CharLit value ->
    literalEvalSr baseName idx (show (fromEnum value)) CharType
  Var varName ->
    variableEvalSr baseName idx varName (typeMap HashMap.! varName)

--   -- recursive cases
--   UnaryOp op subExpr ->
--     let (subSr, subName, newIndex) = genExprEvalSr baseName index subExpr typeMap
--     in unaryOpEvalSr baseName newIndex op subExpr subName
--   BinOp op lhs rhs ->
--     let (lhsSr, lhsName, newIndex) = genExprEvalSr baseName index lhs typeMap
--         (rhsSr, rhsName, finalIndex) = genExprEvalSr baseName newIndex rhs typeMap
--     in binaryOpEvalSr baseName finalIndex op lhsName rhsName

  _ -> error "Unsupported expression type"

-- generates sr for moving lieral into output register
literalEvalSr :: String -> Integer -> String -> DataType -> (String, String, Integer)
literalEvalSr baseName idx value valueType =
  let
    srName = baseName ++ "_" ++ show idx                    -- sr name
    moveInstruction = moveInstr_RegToVar valueType value    -- move instr
    srDef = srName ++ ":\n" ++ moveInstruction ++ "\tret\n" -- sr def

  in (srDef, srName, idx + 1) -- inc idx for next sr inc case of nesting

-- generates sr for moving value within a variable into an output register
variableEvalSr :: String -> Integer -> String -> DataType -> (String, String, Integer)
variableEvalSr baseName idx varName varType =
  let
    srName = baseName ++ "_" ++ show idx                    -- sr name 
    moveInstruction = moveInstr_VarToReg varType varName    -- move instr
    srDef = srName ++ ":\n" ++ moveInstruction ++ "\tret\n" -- sr def

  in (srDef, srName, idx + 1) -- inc idx for next sr inc case of nesting

