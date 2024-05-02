module ParserSpec where

import Test.Hspec
import Parser (parseAssignment, parseDeclaration, ParseError(..))
import Lexer
import AST

-- Test Driver
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
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
      parseAssignment "x" (tail tokens) `shouldBe` Left (InvalidSyntax "Invalid primary expression")

  describe "Variable Declaration Parsing" $ do
    it "parses integer declaration correctly" $ do
      let tokens = [TIdent "x", TSemicolon]
      let expected = Right (Declaration "int" (Var "x"), [])
      parseDeclaration "int" tokens `shouldBe` expected

    it "parses float declaration correctly" $ do
      let tokens = [TIdent "y", TSemicolon]
      let expected = Right (Declaration "float" (Var "y"), [])
      parseDeclaration "float" tokens `shouldBe` expected

    it "parses double declaration correctly" $ do
      let tokens = [TIdent "z", TSemicolon]
      let expected = Right (Declaration "double" (Var "z"), [])
      parseDeclaration "double" tokens `shouldBe` expected

    it "fails on missing semicolon in declaration" $ do
      let tokens = [TIdent "x"]
      let expected = Left (InvalidSyntax "Expected identifier followed by a semicolon for declaration")
      parseDeclaration "int" tokens `shouldBe` expected

    it "fails on extra tokens after declaration" $ do
      let tokens = [TIdent "x", TSemicolon, TIdent "extra"]
      let expected = Left (InvalidSyntax "Expected identifier followed by a semicolon for declaration")
      parseDeclaration "int" tokens `shouldBe` expected
