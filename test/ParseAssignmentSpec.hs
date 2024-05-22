module ParseAssignmentSpec where

import Test.Hspec
import Parser_Main
import Parser_Helper
import Parser_Statements
import Parser_Expressions
import Lexer
import AST

-- Test Driver
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  -- Unit tests for Assignments Parsing
  describe "Literal and Variable Assignment Statement Parsing" $ do

    it "parses simple integer assignments correctly" $ do
      let tokens = [TIdent "x", TAssign, TIntLit 10, TSemicolon]
      let expected = Right (AssignStmt "x" (IntLit 10), [])
      parseAssignment "x" (tail tokens) `shouldBe` expected

    it "parses simple float assignments correctly" $ do
      let tokens = [TIdent "y", TAssign, TFloatLit 3.14, TSemicolon]
      let expected = Right (AssignStmt "y" (FloatLit 3.14), [])
      parseAssignment "y" (tail tokens) `shouldBe` expected

    it "parses simple variable assignments correctly" $ do
      let tokens = [TIdent "z", TAssign, TIdent "x", TSemicolon]
      let expected = Right (AssignStmt "z" (Var "x"), [])
      parseAssignment "z" (tail tokens) `shouldBe` expected

    it "fails on missing semicolon in assignment" $ do
      let tokens = [TIdent "x", TAssign, TIntLit 10]
      parseAssignment "x" (tail tokens) `shouldBe` Left MissingSemicolon

    it "fails on invalid right-hand expression" $ do
      let tokens = [TIdent "x", TAssign, TIf, TSemicolon]
      parseAssignment "x" (tail tokens) `shouldBe` Left (InvalidSyntax "parsePrimaryExpr: Invalid primary expression")

  describe "Expression Assignment Parsing" $ do
 
    it "parses simple unary expression assignment" $ do 
        let tokens = [TIdent "x", TAssign, TMinus, TIntLit 10, TSemicolon]
        let expected = Right (AssignStmt "x" (UnaryOp Neg (IntLit 10)), [])        
        parseAssignment "x" (tail tokens) `shouldBe` expected

    it "parses unary expression assingment with nested expressions" $ do 
        let tokens = [TIdent "x", TAssign, TMinus, TLparen, TIntLit 10, TPlus, TIntLit 20, TRparen, TSemicolon]
        let expected = Right (AssignStmt "x" (UnaryOp Neg (BinOp Add (IntLit 10) (IntLit 20))), [])        
        parseAssignment "x" (tail tokens) `shouldBe` expected

    it "parses simple binary expression" $ do 
        let tokens = [TIdent "x", TAssign, TIntLit 10, TPlus, TIntLit 20, TSemicolon]
        let expected = Right (AssignStmt "x" (BinOp Add (IntLit 10) (IntLit 20)), [])        
        parseAssignment "x" (tail tokens) `shouldBe` expected

    it "parses binary expression with nested bianry expression" $ do
        let tokens = [TIdent "x", TAssign, TIntLit 10, TPlus, TLparen, TIntLit 20, TMinus, TIntLit 5, TRparen, TSemicolon]
        let expected = Right (AssignStmt "x" (BinOp Add (IntLit 10) (BinOp Subtract (IntLit 20) (IntLit 5))), [])        
        parseAssignment "x" (tail tokens) `shouldBe` expected

    it "parses binary expression with nested unary expression" $ do
        let tokens = [TIdent "x", TAssign, TIntLit 10, TPlus, TMinus, TIntLit 20, TSemicolon]
        let expected = Right (AssignStmt "x" (BinOp Add (IntLit 10) (UnaryOp Neg (IntLit 20))), [])        
        parseAssignment "x" (tail tokens) `shouldBe` expected

