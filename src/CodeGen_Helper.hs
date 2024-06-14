module CodeGen_Helper where 

import AST
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Hashable
import Data.List (foldl')

-- type alias for data types
data DataType = IntType | FloatType | DoubleType | CharType | VoidType
  deriving (Eq, Show) 

-- type alias for names of floats in .data section used for intermediate computation
-- maps string floats to their unique label name in memory
type FloatMap = HashMap String String

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

-- Recursively determines the type of an expr. Literal and Variable expr are the base case. Variable expr require
-- lookup within the type map to determine their type. For Unary and Binary expr a depth first search of the expr 
-- AST is used until the first literal or variable is found. Syntax is assumed to be correct. 
getExprType :: Expr -> TypeMap -> DataType 
getExprType expr typeMap = case expr of  

  -- literals (base case)
  IntLit _ -> IntType
  FloatLit _ -> FloatType
  DoubleLit _ -> DoubleType
  CharLit _ -> CharType

  -- variables (base case)
  Var name -> case HashMap.lookup name typeMap of
    Just varType -> varType
    Nothing -> error "Variable not found in type map"

  -- unary and binary ops (recursive case)
  UnaryOp _ subExpr -> getExprType subExpr typeMap
  BinOp _ lhs _ -> getExprType lhs typeMap -- only lhs need be checked
  
  _ -> error "Unsupported expression type"



-- func to gen instruction for moving value in output register into a variable for a specific expr type 
moveInstr_RegToVar :: DataType -> String -> String
moveInstr_RegToVar exprType variable = case exprType of
  IntType -> "\tmov [" ++ variable ++ "], rax\n"       -- int uses rax
  CharType -> "\tmov [" ++ variable ++ "], al\n"       -- char uses al (lower 8 bits of rax)
  FloatType -> "\tmovss [" ++ variable ++ "], xmm0\n"  -- float uses xmm0
  DoubleType -> "\tmovsd [" ++ variable ++ "], xmm1\n" -- double uses xmm1
  _ -> error "Unsupported expression type"

-- func to gen instruction for moving value in a variable into an output register for a specific expr type
moveInstr_VarToReg :: DataType -> String -> String
moveInstr_VarToReg exprType variable = case exprType of
  IntType -> "\tmov rax, [" ++ variable ++ "]\n"       -- int uses rax
  CharType -> "\tmov al, [" ++ variable ++ "]\n"       -- char uses al (lower 8 bits of rax)
  FloatType -> "\tmovss xmm0, [" ++ variable ++ "]\n"  -- float uses xmm0
  DoubleType -> "\tmovsd xmm1, [" ++ variable ++ "]\n" -- double uses xmm1
  _ -> error "Unsupported expression type"


-- func to gen instruction for moving a literal into a type specific output register
-- This requires a slight refactor/addition to the .data section bc we will need to 
-- store float literals in .data bc we cannot directly move them into xmm registers
moveInstr_LitToReg :: DataType -> String -> FloatMap -> String
moveInstr_LitToReg exprType literal floatMap = 
  case exprType of
    IntType -> "\tmov rax, " ++ literal ++ "\n"
    CharType -> "\tmov al, " ++ literal ++ "\n"
    FloatType -> "\tmovss xmm0, [" ++ floatVar ++ "]\n" 
    DoubleType -> "\tmovsd xmm1, [" ++ floatVar ++ "]\n" 
    _ -> error "Unsupported expression type"
  where 
    floatVar = floatMap HashMap.! literal


-- func for pusing val in output register onto the stack, places value in rax/rsp depending on data type
pushToStack :: DataType -> String
pushToStack dataType = case dataType of
  IntType -> "\tpush rax\n"
  CharType -> "\tpush rax\n"
  FloatType -> "\tsub rsp, 4        ; Allocate stack space for the float" ++ 
               "\tmovss [rsp], xmm0 ; Move the float from xmm0 to the stack"
  DoubleType -> "\tsub rsp, 8        ; Allocate stack space for the double" ++ 
                "\tmovsd [rsp], xmm0 ; Move the double from xmm0 to the stack"
  _ -> error "Unsupported data type"

-- func for popping val from stack into output register, places value in rax/xmm1 depending on data type
-- xmm0 is assumed to be used for another value, so xmm1 is used for the pop
popFromStack :: DataType -> String
popFromStack dataType = case dataType of
  IntType -> "\tpop rax\n"
  CharType -> "\tpop rax\n"
  FloatType ->
    "\tmovss xmm1, [rsp]    ; Move the single-precision float from stack top to xmm1\n" ++
    "\tadd rsp, 4           ; Deallocate the stack space for the float\n"
  DoubleType ->
    "\tmovsd xmm1, [rsp]    ; Move the double-precision float from stack top to xmm1\n" ++
    "\tadd rsp, 8           ; Deallocate the stack space for the double\n"
  _ -> error "Unsupported data type"
