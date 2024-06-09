
module CodeGen_Declarations where

import AST
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Hashable
import Data.List (foldl')
import CodeGen_Helper


-- Filters declaration stmts on if should be declared in .data or .bss section
filterDeclarations :: [Stmt] -> ([Stmt], [Stmt])
filterDeclarations stmts = (dataSectionStmts, bssSectionStmts)
  where
    -- collect statements appropriate for .bss and .data sections into lists 
    dataSectionStmts = filter isLiteralDeclaration stmts
    bssSectionStmts = filter isVarOrExprDeclaration stmts

    -- literal stmts belong to .data section
    isLiteralDeclaration :: Stmt -> Bool
    isLiteralDeclaration (DeclarationAssignment _ _ (IntLit _)) = True
    isLiteralDeclaration (DeclarationAssignment _ _ (FloatLit _)) = True
    isLiteralDeclaration (DeclarationAssignment _ _ (DoubleLit _)) = True
    isLiteralDeclaration (DeclarationAssignment _ _ (CharLit _)) = True
    isLiteralDeclaration _ = False

    -- variable, expression, and simple declarations belong to .bss section
    isVarOrExprDeclaration :: Stmt -> Bool
    isVarOrExprDeclaration (DeclarationAssignment _ _ (Var _)) = True
    isVarOrExprDeclaration (DeclarationAssignment _ _ (BinOp _ _ _)) = True
    isVarOrExprDeclaration (DeclarationAssignment _ _ (UnaryOp _ _)) = True
    isVarOrExprDeclaration (SimpleDeclaration _ _) = True

    isVarOrExprDeclaration _ = False


-- generates .data section of the assembly code from a list of stmts. The input statements
-- are expected to have already been filted into .data section appropriate statements
genDataSection :: [Stmt] -> String
genDataSection stmts = foldl' appendDataSection "section .data\n" stmts ++ "\n"
  where
    -- Appends a NASM .data section line for each declaration with a literal
    appendDataSection :: String -> Stmt -> String
    appendDataSection acc stmt = case stmt of
        DeclarationAssignment _ varName expr ->
            case expr of
                IntLit value -> acc ++ "\t" ++ varName ++ " dd " ++ show value ++ "\t; 32-bit int\n"                       
                FloatLit value -> acc ++ "\t" ++ varName ++ " dd " ++ show value ++ "\t; 32-bit single-precision float\n" 
                DoubleLit value -> acc ++ "\t" ++ varName ++ " dq " ++ show value ++ "\t; 64-bit double-precision float\n"
                CharLit value -> acc ++ "\t" ++ varName ++ " db " ++ show (fromEnum value) ++ "\t; Byte for character\n"  
                _ -> acc 
        _ -> acc  -- Ignore non-literal expressions


-- generates .bss section of the assembly code from a list of stmts. The input statements
-- are expected to have already been filted into .bss section appropriate statements
genBssSection stmts typeMap = foldl' (appendBssSection typeMap) "section .bss\n" stmts ++ "\n"
  where
    appendBssSection :: TypeMap -> String -> Stmt -> String
    appendBssSection tMap acc stmt =
      let varName = getVarName stmt
      in case HashMap.lookup varName tMap of
        Just IntType -> acc ++ "\t" ++ varName ++ " resd 1\t; 32-bit int\n"
        Just FloatType -> acc ++ "\t" ++ varName ++ " resd 1\t; 32-bit single-precision float\n"
        Just DoubleType -> acc ++ "\t" ++ varName ++ " resq 1\t; 64-bit double-precision float\n"
        Just CharType -> acc ++ "\t" ++ varName ++ " resb 1\t; Byte for character\n"
        _ -> acc  -- Handle unknown data types or missing entries quietly

    -- get var name
    getVarName :: Stmt -> String
    getVarName (DeclarationAssignment _ varName _) = varName
    getVarName (SimpleDeclaration _ (Var varName)) = varName
    getVarName _ = ""  