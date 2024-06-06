
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

-- getTypeList perfroms a left fold over the parsed program, collecting a list  
-- of (varName, type) tuples for each declaration statement within the program
getTypeMap :: [Stmt] -> TypeMap
getTypeMap stmts = makeTypeMap stmts
  where
    -- get list of all types in prgm and convert to hashmap
    makeTypeMap :: [Stmt] -> TypeMap
    makeTypeMap program = HashMap.fromList (foldl' getType [] program)

    -- accumulates a list of (varName, type) tuples for each declaration statement
    getType :: [(String, DataType)] -> Stmt -> [(String, DataType)]
    getType acc (SimpleDeclaration dataType (Var varName)) = (varName, determineDataType dataType) : acc
    getType acc (DeclarationAssignment dataType varName _) = (varName, determineDataType dataType) : acc
    getType acc _ = acc

    -- determineVarType converts a string data type to a VarType
    determineDataType :: String -> DataType
    determineDataType dataType = case dataType of
      "int" -> IntType
      "float" -> FloatType
      "double" -> DoubleType
      "char" -> CharType
      _ -> error "Unsupported data type"