
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
getTypeMap stmts = (makeTypeMap allDeclarationStmts, allDeclarationStmts)
  where
    allDeclarationStmts = concatMap collectDeclarations stmts -- collect delcarations 
    makeTypeMap = HashMap.fromList . foldl' getType []        -- make type hash map

    -- collects all nested declarations within a statement
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

    -- Takes a declaration statment and gets the var name and data type 
    getType :: [(String, DataType)] -> Stmt -> [(String, DataType)]
    getType acc (SimpleDeclaration dataType (Var varName)) =
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


-- the following functions will iterate through the list of declaration statements, filtering out the 
-- following cases. If the declaration is for a literal, it will be handled by a function that generates
-- the .data section. If the statement is a declaration of either an expression, a variable, or a simple 
-- declaration it will be handled by a function that generates the .bss section. First these different 
-- types of declaratiosn will be separated into their own lists, then the function will return a tuple of 
-- the lists of declarations 


-- Function to categorize declaration statements based on whether 
-- they should be declared within the .data or .bss section
categorizeDeclarations :: [Stmt] -> ([Stmt], [Stmt])
categorizeDeclarations stmts = (dataSectionStmts, bssSectionStmts)
  where
    dataSectionStmts = filter isLiteralDeclaration stmts
    bssSectionStmts = filter isVarOrExprDeclaration stmts

    isLiteralDeclaration :: Stmt -> Bool
    isLiteralDeclaration (DeclarationAssignment _ _ (IntLit _)) = True
    isLiteralDeclaration (DeclarationAssignment _ _ (FloatLit _)) = True
    isLiteralDeclaration (DeclarationAssignment _ _ (DoubleLit _)) = True
    isLiteralDeclaration (DeclarationAssignment _ _ (CharLit _)) = True
    isLiteralDeclaration _ = False

    isVarOrExprDeclaration :: Stmt -> Bool
    isVarOrExprDeclaration (DeclarationAssignment _ _ (Var _)) = True
    isVarOrExprDeclaration (DeclarationAssignment _ _ (BinOp _ _ _)) = True
    isVarOrExprDeclaration (DeclarationAssignment _ _ (UnaryOp _ _)) = True
    isVarOrExprDeclaration (SimpleDeclaration _ _) = True

    isVarOrExprDeclaration _ = False