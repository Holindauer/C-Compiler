module LexerSpec (main, spec) where

import Test.Hspec
import Lexer (lexer, Token(..))

-- Test Driver
main :: IO ()
main = hspec spec

-- Test Cases
spec :: Spec
spec = describe "Lexer" $ do

  describe "Operators and Delimiters" $ do
    it "lexes single character operators" $ do
      lexer "+" `shouldBe` [TPlus, TEOF]
      lexer "-" `shouldBe` [TMinus, TEOF]
      lexer "*" `shouldBe` [TStar, TEOF]
      lexer "/" `shouldBe` [TSlash, TEOF]
    
    it "lexes multi-character operators" $ do
      lexer "==" `shouldBe` [TEqual, TEOF]
      lexer "<=" `shouldBe` [TLessEq, TEOF]
      lexer ">=" `shouldBe` [TGreaterEq, TEOF]
      lexer "!=" `shouldBe` [TNotEqual, TEOF]

  describe "Data Types and Identifiers" $ do
    it "lexes data types and identifiers" $ do
      lexer "int x" `shouldBe` [TInt, TIdent "x", TEOF]
      lexer "double value" `shouldBe` [TDouble, TIdent "value", TEOF]

  describe "Numerical Literals" $ do
    it "lexes integers and floating-point numbers" $ do
      lexer "123" `shouldBe` [TIntLit 123, TEOF]
      lexer "456.789" `shouldBe` [TDoubleLit 456.789, TEOF]

  describe "Control Structures" $ do
    it "lexes control structures" $ do
      lexer "if (x > 0)" `shouldBe` [TIf, TLparen, TIdent "x", TGreaterThan, TIntLit 0, TRparen, TEOF]

  describe "Whitespace Handling" $ do
    it "ignores whitespace" $ do
      lexer "   int    x   " `shouldBe` [TInt, TIdent "x", TEOF]

  describe "Full Assignment Statement" $ do
    it "lexes a full C statement" $ do
      lexer "int x = 5 + 3 * 2;" `shouldBe` [TInt, TIdent "x", TAssign, TIntLit 5, TPlus, TIntLit 3, TStar, TIntLit 2, TSemicolon, TEOF]

  describe "Full Conditional Statment" $ do
    it "lexes a full C statement" $ do
      lexer "if (x > 0) { x = 1; } else { x = 0; }" `shouldBe` [TIf, TLparen, TIdent "x", TGreaterThan, TIntLit 0, TRparen, TLbrace, TIdent "x", TAssign, TIntLit 1, TSemicolon, TRbrace, TElse, TLbrace, TIdent "x", TAssign, TIntLit 0, TSemicolon, TRbrace, TEOF]

