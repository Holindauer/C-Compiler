
module CodeGen_Declarations where

import AST
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Hashable
import Data.List (foldl')



-- type alias for data types
data DataType = IntType | FloatType | DoubleType | CharType | VoidType
  deriving (Eq, Show) 

-- type alias for a hashmap of variable names and their types
type TypeMap = HashMap String DataType

-- getTypeMap takes a list of statements and for each declaration statement, it returns 
-- a hashmap of variable names, their data types, and a list of each declaration stmt.
getTypeMap :: [Stmt] -> (TypeMap, [Stmt])
getTypeMap stmts = (makeTypeMap declarationStmts, declarationStmts)
  where
    -- Filter declaration statements and create hashmap
    declarationStmts = filter isDecl stmts
    makeTypeMap = HashMap.fromList . foldl' getType []

    -- Determine if a statement is a declaration stmt
    isDecl :: Stmt -> Bool
    isDecl (SimpleDeclaration _ _) = True
    isDecl (DeclarationAssignment _ _ _) = True
    isDecl _ = False

    -- Accumulates list of (varName, type) tuples for each declaration statement
    getType :: [(String, DataType)] -> Stmt -> [(String, DataType)]
    getType acc (SimpleDeclaration dataType (Var varName)) =
      (varName, determineDataType dataType) : acc
    getType acc (DeclarationAssignment dataType varName _) =
      (varName, determineDataType dataType) : acc
    getType acc _ = acc

    -- Convert a string data type to a DataType
    determineDataType :: String -> DataType
    determineDataType dataType = case dataType of
      "int" -> IntType
      "float" -> FloatType
      "double" -> DoubleType
      "char" -> CharType
      _ -> error "Unsupported data type"