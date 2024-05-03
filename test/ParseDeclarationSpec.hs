module ParseDeclarationSpec where

import Test.Hspec
import Parser (parseProgram, parseExpr, parseStmt, parseAssignment, parseDeclaration, parseConditional, ParseError(..))
import Lexer
import AST

-- Test Driver
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  -- Unit tests for Variable Declaration Parsing
  describe "Variable Declaration Parsing" $ do
    it "parses simple integer declaration correctly" $ do
      let tokens = [TIdent "x", TSemicolon]
      let expected = Right (SimpleDeclaration "int" (Var "x"), [])
      parseDeclaration "int" tokens `shouldBe` expected

    it "parses simple char declaration correctly" $ do
      let tokens = [TIdent "y", TSemicolon]
      let expected = Right (SimpleDeclaration "char" (Var "y"), [])
      parseDeclaration "char" tokens `shouldBe` expected

    it "parses float declaration with initialization correctly" $ do
      let tokens = [TIdent "f", TAssign, TFloatLit 3.14, TSemicolon]
      let expected = Right (DeclarationAssignment "float" "f" (FloatLit 3.14), [])
      parseDeclaration "float" tokens `shouldBe` expected

    it "parses double declaration with initialization correctly" $ do
      let tokens = [TIdent "d", TAssign, TDoubleLit 2.718, TSemicolon]
      let expected = Right (DeclarationAssignment "double" "d" (DoubleLit 2.718), [])
      parseDeclaration "double" tokens `shouldBe` expected

    it "fails on missing semicolon in simple declaration" $ do
      let tokens = [TIdent "x"]
      let expected = Left (InvalidSyntax "Invalid declaration format")
      parseDeclaration "int" tokens `shouldBe` expected

    it "fails on missing semicolon after initialization" $ do
      let tokens = [TIdent "x", TAssign, TIntLit 10]
      let expected = Left MissingSemicolon
      parseDeclaration "int" tokens `shouldBe` expected

    it "fails on extra tokens after simple declaration" $ do
      let tokens = [TIdent "x", TSemicolon, TIdent "extra"]
      let expected = Left (UnexpectedToken (TIdent "extra"))
      parseDeclaration "int" tokens `shouldBe` expected

    it "fails on extra tokens after declaration with initialization" $ do
      let tokens = [TIdent "x", TAssign, TIntLit 10, TSemicolon, TIdent "extra"]
      let expected = Left (UnexpectedToken (TIdent "extra"))
      parseDeclaration "int" tokens `shouldBe` expected

