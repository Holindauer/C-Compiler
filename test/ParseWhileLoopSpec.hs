module ParseWhileLoopSpec where

import Test.Hspec
import Lexer (Token(..))
import AST (Expr(..), Stmt(..), Op(..))
import Parser_Main
import Parser_Helper
import Parser_Statements
import Parser_Expressions

-- Helper function to generate tokens for a simple while loop
-- Corresponding to: while (x < 5) { x = x + 1; }
simpleWhileTokens :: [Token]
simpleWhileTokens = [
    TWhile, TLparen, TIdent "x", TLessThan, TIntLit 5, TRparen,
    TLbrace, TIdent "x", TAssign, TIdent "x", TPlus, TIntLit 1, TSemicolon, TRbrace
    ]

-- Expected AST for the simple while loop
expectedSimpleWhileAST :: Stmt
expectedSimpleWhileAST = WhileStmt
    (BinOp LessThan (Var "x") (IntLit 5))
    [AssignStmt "x" (BinOp Add (Var "x") (IntLit 1))]

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Parser Tests" $ do
  describe "While Loop Parsing" $ do
    it "parses a simple while loop correctly" $ do
      let result = parseWhileLoop simpleWhileTokens
      result `shouldBe` Right (expectedSimpleWhileAST, [])

    it "handles missing closing parenthesis" $ do
      let tokens = [TWhile, TLparen, TIdent "x", TLessThan, TIntLit 5, TLbrace]
      let result = parseWhileLoop tokens
      result `shouldBe` Left (InvalidSyntax "Expected closing parenthesis")

    it "handles missing opening brace for the loop body" $ do
      let tokens = [TWhile, TLparen, TIdent "x", TLessThan, TIntLit 5, TRparen]
      let result = parseWhileLoop tokens
      result `shouldBe` Left (InvalidSyntax "Expected opening brace")

    it "handles extra tokens after the loop body" $ do
      let tokens = simpleWhileTokens ++ [TIntLit 10] -- Extra token after the loop
      let result = parseWhileLoop tokens
      result `shouldBe` Right (expectedSimpleWhileAST, [TIntLit 10])
