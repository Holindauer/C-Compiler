module ParseConditionalSpec where

import Test.Hspec
import Lexer (Token(..))
import AST (Expr(..), Stmt(..), Op(..))
import Parser (parseConditional, parseExpr, parseParenthesizedExpr, parseBlock, parseOptionalElse, parseStatementsUntilBrace, parseWhileLoop, ParseError(..))

-- Helper functions to create tokens and expected ASTs for testing
simpleIfTokens :: [Token]
simpleIfTokens = [
    TIf, TLparen, TIdent "x", TGreaterThan, TIntLit 0, TRparen,
    TLbrace, TIdent "x", TAssign, TIntLit 1, TSemicolon, TRbrace
  ]

expectedSimpleIfAST :: Stmt
expectedSimpleIfAST = IfStmt
    (BinOp GreaterThan (Var "x") (IntLit 0))
    [AssignStmt "x" (IntLit 1)]
    []

-- Main testing module
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Conditional Parsing Helper Functions" $ do

    it "parseExpr parses the binary expression within conditional correctly" $ do
      let tokens = [TIdent "x", TGreaterThan, TIntLit 0]
      let expected = Right (BinOp GreaterThan (Var "x") (IntLit 0), [])
      parseExpr tokens `shouldBe` expected

    it "parses an expression within parentheses correctly" $ do
      let tokens = [TLparen, TIdent "x", TGreaterThan, TIntLit 0, TRparen]
      let expected = Right (BinOp GreaterThan (Var "x") (IntLit 0), [])
      parseParenthesizedExpr tokens `shouldBe` expected

    it "fails if there is no closing parenthesis" $ do
      let tokens = [TLparen, TIdent "x", TGreaterThan, TIntLit 0]
      parseParenthesizedExpr tokens `shouldBe` Left (InvalidSyntax "Expected closing parenthesis")

    it "parses an else block correctly" $ do
      let tokens = [TElse, TLbrace, TIdent "x", TAssign, TIntLit 2, TSemicolon, TRbrace]
      let expected = Right ([ElseStmt [AssignStmt "x" (IntLit 2)]], [])
      parseOptionalElse tokens `shouldBe` expected

    it "parses an else-if block correctly" $ do
      let tokens = [TElse, TIf, TLparen, TIdent "x", TEqual, TIntLit 2, TRparen, TLbrace, TIdent "x", TAssign, TIntLit 3, TSemicolon, TRbrace]
      let nestedIfStmt = IfStmt (BinOp Equal (Var "x") (IntLit 2)) [AssignStmt "x" (IntLit 3)] []
      let expected = Right ([ElseStmt [nestedIfStmt]], [])
      parseOptionalElse tokens `shouldBe` expected

    it "accumulates multiple statements correctly until a closing brace" $ do
      let tokens = [TIdent "x", TAssign, TIntLit 1, TSemicolon, TIdent "y", TAssign, TIntLit 2, TSemicolon, TRbrace]
      let expected = Right ([AssignStmt "x" (IntLit 1), AssignStmt "y" (IntLit 2)], [TRbrace])
      parseStatementsUntilBrace tokens [] `shouldBe` expected

    it "returns an empty list if no statements are present before a closing brace" $ do
      let tokens = [TRbrace]
      let expected = Right ([], [TRbrace])
      parseStatementsUntilBrace tokens [] `shouldBe` expected

  describe "Full Conditional Parsing" $ do
    it "parses a simple if statement with a single expression in the body correctly" $ do
      let result = parseConditional simpleIfTokens
      result `shouldBe` Right (expectedSimpleIfAST, [])
