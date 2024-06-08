
module CodeGen_Declarations where

import AST
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Hashable
import Data.List (foldl')

-- type alias for data types
data DataType = IntType | FloatType | DoubleType | CharType | VoidType
  deriving (Eq, Show) 

-- type alias for a hashmap of var names and their types
type TypeMap = HashMap String DataType

-- getTypeMap collects each variable declaration from a list of stmts into a seperate list.
-- It returns a hashmap (var name -> type) as well as the list of declaration stmts
getTypeMap :: [Stmt] -> (TypeMap, [Stmt])
getTypeMap stmts = (makeTypeMap allDeclarationStmts, allDeclarationStmts)
  where
    allDeclarationStmts = concatMap collectDeclarations stmts -- collect delcarations 
    makeTypeMap = HashMap.fromList . foldl' getType []        -- func that makes type hash map

    -- recursively collects all nested declarations within a statement
    collectDeclarations :: Stmt -> [Stmt]

    -- declarations (base case)
    collectDeclarations stmt@(SimpleDeclaration _ _) = [stmt] 
    collectDeclarations stmt@(DeclarationAssignment _ _ _) = [stmt]

    -- statements w/ stmt bodies (recursive case) 
    collectDeclarations (ForStmt initStmt _ _ body) =
      collectDeclarations initStmt ++ concatMap collectDeclarations body 
    collectDeclarations (WhileStmt _ body) = 
      concatMap collectDeclarations body
    collectDeclarations (IfStmt _ thenBody elseBody) =
      concatMap collectDeclarations thenBody ++ concatMap collectDeclarations elseBody
    collectDeclarations _ = []

    -- accepts a list of (var name, type) tuples and a declaration stmt. 
    -- A tuple with that stmts var name and type is appended to the list
    getType :: [(String, DataType)] -> Stmt -> [(String, DataType)]
    getType acc (SimpleDeclaration dataType (Var varName)) = -- 
      (varName, determineDataType dataType) : acc
    getType acc (DeclarationAssignment dataType varName _) =
      (varName, determineDataType dataType) : acc
    getType acc _ = acc

    -- converts string data type to DataType
    determineDataType :: String -> DataType
    determineDataType dataType = case dataType of
      "int" -> IntType
      "float" -> FloatType
      "double" -> DoubleType
      "char" -> CharType
      _ -> error "Unsupported data type"

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