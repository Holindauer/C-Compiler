{-# LANGUAGE OverloadedStrings #-}

module LexerThenParserSpec where

import Test.Hspec
import Lexer
import Parser_Main
import Parser_Helper
import Parser_Statements
import Parser_Expressions
import AST

-- Test Driver
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Lexer and Parser Integration" $ do
    -- Test case for a simple integer declaration
    it "parses a simple integer declaration" $ do
      let sourceCode = "int main(void) { int x; return 0; }"
      let tokens = lexer sourceCode
      let expectedTokens = [TInt, TMain, TLparen, TVoid, TRparen, TLbrace, TInt, TIdent "x", TSemicolon, TReturn, TIntLit 0, TSemicolon, TRbrace, TEOF]
      tokens `shouldBe` expectedTokens

      let parsed = parseProgram tokens
      let expectedAst = Right ([SimpleDeclaration "int" (Var "x")], [])
      parsed `shouldBe` expectedAst

    -- Test case for a declaration with an assignment
    it "parses an integer declaration with an assignment" $ do
      let sourceCode = "int main(void) { int x = 5; return 0; }"
      let tokens = lexer sourceCode
      let expectedTokens = [TInt, TMain, TLparen, TVoid, TRparen, TLbrace, TInt, TIdent "x", TAssign, TIntLit 5, TSemicolon, TReturn, TIntLit 0, TSemicolon, TRbrace, TEOF]
      tokens `shouldBe` expectedTokens

      let parsed = parseProgram tokens
      let expectedAst = Right ([DeclarationAssignment "int" "x" (IntLit 5)], [])
      parsed `shouldBe` expectedAst

    -- Test case for a conditional statement with a block
    it "parses a simple if statement with a block" $ do
      let sourceCode = "int main(void) { if (x < 5) { x = x + 1; } return 0; }"
      let tokens = lexer sourceCode
      let expectedTokens = [TInt, TMain, TLparen, TVoid, TRparen, TLbrace, TIf, TLparen, TIdent "x", TLessThan, TIntLit 5, TRparen, TLbrace, TIdent "x", TAssign, TIdent "x", TPlus, TIntLit 1, TSemicolon, TRbrace, TReturn, TIntLit 0, TSemicolon, TRbrace, TEOF]
      tokens `shouldBe` expectedTokens

      let parsed = parseProgram tokens
      let expectedAst = Right ([IfStmt (BinOp LessThan (Var "x") (IntLit 5)) [AssignStmt "x" (BinOp Add (Var "x") (IntLit 1))] []], [])
      parsed `shouldBe` expectedAst

    -- Test case for a simple while loop
    it "parses a simple while loop" $ do
      let sourceCode = "int main(void) { while (x < 5) { x = x + 1; } return 0; }"
      let tokens = lexer sourceCode
      let expectedTokens = [TInt, TMain, TLparen, TVoid, TRparen, TLbrace, TWhile, TLparen, TIdent "x", TLessThan, TIntLit 5, TRparen, TLbrace, TIdent "x", TAssign, TIdent "x", TPlus, TIntLit 1, TSemicolon, TRbrace, TReturn, TIntLit 0, TSemicolon, TRbrace, TEOF]
      tokens `shouldBe` expectedTokens

      let parsed = parseProgram tokens
      let expectedAst = Right ([WhileStmt (BinOp LessThan (Var "x") (IntLit 5)) [AssignStmt "x" (BinOp Add (Var "x") (IntLit 1))]], [])
      parsed `shouldBe` expectedAst


    -- Test case for a simple for loop
    it "parses a simple for loop" $ do
      let sourceCode = "int main(void) { for (int i = 0; i < 5; i++) { x = x + 1; } return 0; }"
      let tokens = lexer sourceCode
      let expectedTokens = [TInt, TMain, TLparen, TVoid, TRparen, TLbrace, TFor, TLparen, TInt, TIdent "i", TAssign, TIntLit 0, TSemicolon, TIdent "i", TLessThan, TIntLit 5, TSemicolon, TIdent "i", TIncrement, TRparen, TLbrace, TIdent "x", TAssign, TIdent "x", TPlus, TIntLit 1, TSemicolon, TRbrace, TReturn, TIntLit 0, TSemicolon, TRbrace, TEOF]
      tokens `shouldBe` expectedTokens
      
      let parsed = parseProgram tokens
      let expectedAst = Right ([ForStmt (DeclarationAssignment "int" "i" (IntLit 0)) (BinOp LessThan (Var "i") (IntLit 5)) (IncrementStmt "i") [AssignStmt "x" (BinOp Add (Var "x") (IntLit 1))]], [])
      parsed `shouldBe` expectedAst


    -- Test case for small programm
    ---
    --- int x = 10;
    --- float y = 20;
    --- for (int i = 0; i < 5; i++) { x = x + x; } 
    --- if (x < 5) { x = x + 1; }
    --- while (y > 0) { y = y - 1; }
    ---
    it "parses a small program" $ do
        let sourceCode = "int main(void) { int x = 10; float y = 20; for (int i = 0; i < 5; i++) { x = x + x; } if (x < 5) { x = x + 1; } while (y > 0) { y = y - 1; } return 0; }"
        let tokens = lexer sourceCode
        let parsed = parseProgram tokens
        let expectedAst = Right ([DeclarationAssignment "int" "x" (IntLit 10)
                                  , DeclarationAssignment "float" "y" (IntLit 20) 
                                  , ForStmt (DeclarationAssignment "int" "i" (IntLit 0)) (BinOp LessThan (Var "i") (IntLit 5)) (IncrementStmt "i") [AssignStmt "x" (BinOp Add (Var "x") (Var "x"))]
                                  , IfStmt (BinOp LessThan (Var "x") (IntLit 5)) [AssignStmt "x" (BinOp Add (Var "x") (IntLit 1))] []
                                  , WhileStmt (BinOp GreaterThan (Var "y") (IntLit 0)) [AssignStmt "y" (BinOp Subtract (Var "y") (IntLit 1))]]
                                  , [])


        parsed `shouldBe` expectedAst
