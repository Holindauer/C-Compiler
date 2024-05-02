module ParserSpec where

import Test.Hspec
import Parser (parseAssignment, parseProgram, parseExpr, parseStmt, ParseError(..))
import Lexer
import AST

-- Test Driver
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Assignment Statement Parsing" $ do
    -- Assuming correct lexing of the statement: "x = 10;"
    it "parses simple integer assignments correctly" $ do
      let tokens = [TIdent "x", TAssign, TIntLit 10, TSemicolon]
      let expected = Right (AssignStmt "x" (IntLit 10), [])
      parseAssignment "x" (tail tokens) `shouldBe` expected

    -- Assuming correct lexing of the statement: "y = 3.14;"
    it "parses simple float assignments correctly" $ do
      let tokens = [TIdent "y", TAssign, TFloatLit 3.14, TSemicolon]
      let expected = Right (AssignStmt "y" (FloatLit 3.14), [])
      parseAssignment "y" (tail tokens) `shouldBe` expected

    -- Assuming correct lexing of the statement: "z = x;"
    it "parses simple variable assignments correctly" $ do
      let tokens = [TIdent "z", TAssign, TIdent "x", TSemicolon]
      let expected = Right (AssignStmt "z" (Var "x"), [])
      parseAssignment "z" (tail tokens) `shouldBe` expected

    -- Assuming incorrect lexing of the statement: "x = 10"
    it "fails on missing semicolon in assignment" $ do
      let tokens = [TIdent "x", TAssign, TIntLit 10]
      parseAssignment "x" (tail tokens) `shouldBe` Left MissingSemicolon

    -- Assuming lexing of an invalid right-hand expression: "x = if;"
    it "fails on invalid right-hand expression" $ do
        let tokens = [TIdent "x", TAssign, TIf, TSemicolon]
        parseAssignment "x" (tail tokens) `shouldBe` Left (InvalidSyntax "Invalid primary expression")

