module ParserSpec where

-- ParserSpec.hs contains the test cases for the parser functions

import Test.Hspec
import Parser (parseAssignment, parseProgram, parseExpr, parseStmt)
import Lexer
import AST

-- Test Driver
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Assignment Statement Parsing" $ do

    -- assuming correct lexing of the statment : "x = 10;"
    it "parses simple integer assignments correctly" $ do
      let tokens = [TIdent "x", TAssign, TIntLit 10, TSemicolon]
          expected = Just (AssignStmt "x" (IntLit 10))
      fst (parseAssignment "x" (tail tokens)) `shouldBe` expected

    -- assuming correct lexing of the statment : "y = 3.14;"
    it "parses simple float assignments correctly" $ do
      let tokens = [TIdent "y", TAssign, TFloatLit 3.14, TSemicolon]
          expected = Just (AssignStmt "y" (FloatLit 3.14))
      fst (parseAssignment "y" (tail tokens)) `shouldBe` expected

    -- assuming correct lexing of the statment : "z = x;"
    it "parses simple variable assignments correctly" $ do
      let tokens = [TIdent "z", TAssign, TIdent "x", TSemicolon]
          expected = Just (AssignStmt "z" (Var "x"))
      fst (parseAssignment "z" (tail tokens)) `shouldBe` expected

    -- assuming correct lexing of the statment : "x = 10 + 5;"
    it "fails on missing semicolon in assignment" $ do
      let tokens = [TIdent "x", TAssign, TIntLit 10]
      fst (parseAssignment "x" (tail tokens)) `shouldBe` Nothing

    -- -- assuming correct lexing of the statment : "x = if;"
    -- it "fails on invalid right-hand expression" $ do
    --   let tokens = [TIdent "x", TAssign, TIf, TSemicolon]  -- 'if' is not a valid expression
    --   fst (parseAssignment "x" (tail tokens)) `shouldBe` Nothing
