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
  describe "Assignment Statement Parsing" $ do
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

