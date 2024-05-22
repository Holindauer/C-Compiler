module ParseReturnStatementSpec where

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

    -- Unit tests for Return Statment Parsing
    describe "Return Statement Parsing" $ do

        -- int literal
        it "Parses return 0; / integer Statement" $ do
            let tokens = [TReturn, TIntLit 0, TSemicolon]
            let expected = Right (ReturnStmt (IntLit 0), [])
    
            parseReturnStmt tokens `shouldBe` expected

        -- char literal
        it "Parses Return char Statement" $ do
            let tokens = [TReturn, TCharLit 'a', TSemicolon]
            let expected = Right (ReturnStmt (CharLit 'a'), [])

            parseReturnStmt tokens `shouldBe` expected

        -- float literal
        it "Parses Return float Statement" $ do
            let tokens = [TReturn, TFloatLit 9.1, TSemicolon]
            let expected = Right (ReturnStmt (FloatLit 9.1), [])

            parseReturnStmt tokens `shouldBe` expected

        -- double literal 
        it "Parses Return char Statement" $ do
            let tokens = [TReturn, TDoubleLit 2.2, TSemicolon]
            let expected = Right (ReturnStmt (DoubleLit 2.2), [])

            parseReturnStmt tokens `shouldBe` expected

        -- char literal
        it "Parses Return char Statement" $ do
            let tokens = [TReturn, TIdent "variable", TSemicolon]
            let expected = Right (ReturnStmt (Var "variable"), [])

            parseReturnStmt tokens `shouldBe` expected

         -- variable
        it "Parses Return char Statement" $ do
            let tokens = [TReturn, TIdent "variable", TSemicolon]
            let expected = Right (ReturnStmt (Var "variable"), [])

            parseReturnStmt tokens `shouldBe` expected

        -- unary op
        it "Parses Return unary op Statement" $ do
            let tokens = [TReturn, TNot, TIdent "variable", TSemicolon]
            let expected = Right (ReturnStmt (UnaryOp LogicalNot (Var "variable")), [])

            parseReturnStmt tokens `shouldBe` expected

        -- binary op
        it "Parses Return binary op Statement" $ do
            let tokens = [TReturn, TIdent "variable", TMinus, TIntLit 1, TSemicolon]
            let expected = Right (ReturnStmt (BinOp Subtract (Var "variable") (IntLit 1)), [])

            parseReturnStmt tokens `shouldBe` expected


            

