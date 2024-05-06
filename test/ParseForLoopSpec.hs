module ParseForLoopSpec where

import Test.Hspec
import Lexer (Token(..))
import AST (Expr(..), Stmt(..), Op(..))
import Parser (parseForLoop, parseForLoopHeader, parseDeclaration, parseUpdateStatement, extractForLoopHeader, ensureSemicolon)
import Data.Either (isLeft)

spec :: Spec
spec = do
  describe "extractForLoopHeader" $ do

    -- Note: extractForLoopHeader will extract all tokens from inside the parentheses, returning them without the parentheses
    it "successfully extracts content inside parentheses" $ do
      let tokens = [TLparen, TInt, TIdent "i", TAssign, TIntLit 0, TSemicolon, TIdent "i", TLessThan, TIntLit 10, TSemicolon, TIdent "i", TIncrement, TRparen, TLbrace]
      extractForLoopHeader tokens `shouldBe` Right ([TInt, TIdent "i", TAssign, TIntLit 0, TSemicolon, TIdent "i", TLessThan, TIntLit 10, TSemicolon, TIdent "i", TIncrement], [TLbrace])

    it "reports error on missing closing parenthesis" $ do
      let tokens = [TInt, TIdent "i", TAssign, TIntLit 0, TSemicolon, TIdent "i", TLessThan, TIntLit 10, TSemicolon, TIdent "i", TIncrement, TLbrace]
      extractForLoopHeader (drop 2 tokens) `shouldSatisfy` isLeft

  -- Note: parseForLoopHeader is expecting the tokens extracted by extractForLoopHeader. There should not be parentheses around these tokens
  describe "parseForLoopHeader" $ do

    it "parseDeclaration succesully parses a declaration with tokens after semicolon" $ do
      let tokens = [TIdent "i", TAssign, TIntLit 0, TSemicolon, TIdent "i", TLessThan, TIntLit 10, TSemicolon, TIdent "i", TIncrement]
      parseDeclaration "int" tokens `shouldBe` Right (DeclarationAssignment "int" "i" (IntLit 0), [TIdent "i", TLessThan, TIntLit 10, TSemicolon, TIdent "i", TIncrement])




    it "parses valid for loop headers correctly" $ do
      let tokens = [TInt, TIdent "i", TAssign, TIntLit 0, TSemicolon, TIdent "i", TLessThan, TIntLit 10, TSemicolon, TIdent "i", TIncrement]
      parseForLoopHeader tokens `shouldBe` Right (DeclarationAssignment "int" "i" (IntLit 0), BinOp LessThan (Var "i") (IntLit 10), IncrementStmt "i")



    it "throws error on missing semicolon" $ do
      let tokens = [TInt, TIdent "i", TAssign, TIntLit 0, TIdent "i", TLessThan, TIntLit 10, TSemicolon, TIdent "i", TIncrement]
      parseForLoopHeader tokens `shouldSatisfy` isLeft

  describe "parseUpdateStatement" $ do
    it "parses post-increment correctly" $ do
      let tokens = [TIdent "i", TIncrement]
      parseUpdateStatement tokens `shouldBe` Right (IncrementStmt "i", [])

    it "parses post-decrement correctly" $ do
      let tokens = [TIdent "i", TDecrement]
      parseUpdateStatement tokens `shouldBe` Right (DecrementStmt "i", [])

    it "parses pre-decrement correctly" $ do
      let tokens = [TDecrement, TIdent "i"]
      parseUpdateStatement tokens `shouldBe` Right (DecrementStmt "i", [])

    it "parses pre-increment correctly" $ do
      let tokens = [TIncrement, TIdent "i"]
      parseUpdateStatement tokens `shouldBe` Right (IncrementStmt "i", [])

    it "fails on invalid operation" $ do
      let tokens = [TIdent "i", TPlusAssign, TIntLit 1]
      parseUpdateStatement tokens `shouldSatisfy` isLeft

  describe "parseForLoop" $ do
    it "parses a complete for loop correctly" $ do
      let tokens = [TFor, TLparen, TInt, TIdent "i", TAssign, TIntLit 0, TSemicolon, TIdent "i", TLessThan, TIntLit 10, TSemicolon, TIdent "i", TIncrement, TRparen, TLbrace, TRbrace]
      parseForLoop tokens `shouldBe` Right (ForStmt (DeclarationAssignment "int" "i" (IntLit 0)) (BinOp LessThan (Var "i") (IntLit 10)) (IncrementStmt "i") [], [TRbrace])

