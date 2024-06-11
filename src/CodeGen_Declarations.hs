
module CodeGen_Declarations where

import AST
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Hashable
import Data.List (foldl')
import CodeGen_Helper

-------------------------------------------------------------------------------------------------- .data and .bss section stmt sort

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

-------------------------------------------------------------------------------------------------- .data section generation 

-- Generates the .data section of assembly code from a list of statements
genDataSection :: [Stmt] -> Program -> (String, HashMap String String)
genDataSection stmts program =
    let
        -- Begin .data by appending declarations
        declarations = foldl' appendDeclarations "section .data\n" stmts

        -- Find all float and double literals in the program, assinging each a unique name 
        namedFloats = getFloatDoubleLits program

        -- Appen intermediate float literals to the .data section
        fullDataSection = appendIntermediateFloats declarations namedFloats ++ "\n"

        -- Create a map of float literals to their names
        floatMap = HashMap.fromList $ map (\(num, name, _) -> (num, name)) namedFloats

    in (fullDataSection, floatMap)

-- Appends data section lines for each declaration with a literal
appendDeclarations :: String -> Stmt -> String
appendDeclarations acc (DeclarationAssignment dataType varName expr) =
    case expr of
        IntLit value -> acc ++ "\t" ++ varName ++ " dd " ++ show value ++ " ; 32-bit int\n"
        FloatLit value -> acc ++ "\t" ++ varName ++ " dd " ++ show value ++ " ; 32-bit single-precision float\n"
        DoubleLit value -> acc ++ "\t" ++ varName ++ " dq " ++ show value ++ " ; 64-bit double-precision float\n"
        CharLit value -> acc ++ "\t" ++ varName ++ " db " ++ show (fromEnum value) ++ " ; Byte for character\n"
        _ -> acc
appendDataSection acc _ = acc

-- Adds intermediate floating point literals to the .data section
appendIntermediateFloats :: String -> [(String, String, DataType)] -> String
appendIntermediateFloats acc floatData =
    foldl' appendFloatData acc floatData
  where
    -- Appends a single float literal declaration to the .data section
    appendFloatData acc (num, floatName, dataType) =
        case dataType of
            FloatType -> acc ++ "\t" ++ floatName ++ " dd " ++ num ++ " ; 32-bit single-precision float\n"
            DoubleType -> acc ++ "\t" ++  floatName ++ " dq " ++ num ++ " ; 64-bit double-precision float\n"
            _ -> acc

-- Retrieves and names all float/double literals from a list of statements
getFloatDoubleLits :: [Stmt] -> [(String, String, DataType)] -- [(literal as str, name, type)]
getFloatDoubleLits stmts =
    let
        -- Get all float literals from the program
        floatLits = concatMap getFloatLits stmts
        namedFloats = zipWith (\i (num, dtype) -> (num, "float_" ++ show i, dtype)) [1..] floatLits
    in namedFloats

-- Helper to get float literals used in intermediate computation from each type of statement
getFloatLits :: Stmt -> [(String, DataType)]
getFloatLits (DeclarationAssignment _ _ expr) = getExprFloats expr
getFloatLits (AssignStmt _ expr) = getExprFloats expr
getFloatLits (ForStmt init _ _ body) = getFloatLits init ++ concatMap getFloatLits body
getFloatLits (WhileStmt _ body) = concatMap getFloatLits body
getFloatLits (IfStmt _ thenBody elseBody) = concatMap getFloatLits thenBody ++ concatMap getFloatLits elseBody
getFloatLits _ = []

-- Recursively finds float literals within expressions
getExprFloats :: Expr -> [(String, DataType)]
getExprFloats (FloatLit value) = [(show value, FloatType)]
getExprFloats (DoubleLit value) = [(show value, DoubleType)]
getExprFloats _ = []

-------------------------------------------------------------------------------------------------- .bss section generation


-- generates .bss section of the assembly code from a list of stmts. The input statements
-- are expected to have already been filted into .bss section appropriate statements
genBssSection stmts typeMap = foldl' (appendBssSection typeMap) "section .bss\n" stmts ++ "\n\n"
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