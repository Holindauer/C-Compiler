module CodeGeneratorBssSpec (main, spec) where

import Test.Hspec
import AST
import CodeGenerator

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "generateBssSection with one variable declaration of each type" $ do

    it "generates correct .bss section for simple int declaration" $ do
      let stmts = [SimpleDeclaration "int" (Var "x")]
      generateBssSection stmts `shouldBe` "\tx_label: resd 1\n"

    it "generates correct .bss section for multiple declarations" $ do
      let stmts = [SimpleDeclaration "int" (Var "x"), SimpleDeclaration "double" (Var "y")]
      generateBssSection stmts `shouldBe` "\tx_label: resd 1\n\ty_label: resq 1\n"

    it "handles declarations with initial values" $ do
      let stmts = [DeclarationAssignment "float" "z" (IntLit 0)]
      generateBssSection stmts `shouldBe` "\tz_label: resd 1\n"

    it "ignores non-declaration statements" $ do
      let stmts = [ExprStmt (IntLit 5), SimpleDeclaration "char" (Var "c")]
      generateBssSection stmts `shouldBe` "\tc_label: resb 1\n"

  describe "generateBssSection with multiple variable declarations interspersed throughout statement list" $ do

    it "generates correct .bss section for multiple declarations" $ do
      let stmts = [SimpleDeclaration "int" (Var "x"), ExprStmt (IntLit 5), SimpleDeclaration "double" (Var "y")]
      generateBssSection stmts `shouldBe` "\tx_label: resd 1\n\ty_label: resq 1\n"

    it "handles declarations with initial values" $ do
      let stmts = [SimpleDeclaration "int" (Var "x"), DeclarationAssignment "float" "z" (IntLit 0)]
      generateBssSection stmts `shouldBe` "\tx_label: resd 1\n\tz_label: resd 1\n"

    it "ignores non-declaration statements" $ do
      let stmts = [SimpleDeclaration "int" (Var "x"), ExprStmt (IntLit 5), SimpleDeclaration "char" (Var "c")]
      generateBssSection stmts `shouldBe` "\tx_label: resd 1\n\tc_label: resb 1\n"

    -- Under construction

  -- describe "generate with generateCode to include .bss header" $ do

    -- it "generates full assembly code with .bss section" $ do
    --   let program = [SimpleDeclaration "int" (Var "x"), SimpleDeclaration "double" (Var "y")]
    --   let expected = "section .bss\n\tx_label: resd 1\n\ty_label: resq 1\nsection .text\n"
    --   generateCode program `shouldBe` expected

  describe "Variable Declarations in Nested Structures" $ do
    it "extracts declarations within For loop bodies" $ do
      let stmts = [ForStmt (SimpleDeclaration "int" (Var "i")) (Var "i < 10") (AssignStmt "i" (BinOp Add (Var "i") (IntLit 1))) 
                    [DeclarationAssignment "double" "z" (DoubleLit 0.0)]]
      generateBssSection stmts `shouldBe` "\ti_label: resd 1\n\tz_label: resq 1\n"

    it "extracts declarations within While loop bodies" $ do
      let stmts = [WhileStmt (BinOp LessThan (Var "x") (IntLit 20)) [SimpleDeclaration "char" (Var "c"), DeclarationAssignment "float" "f" (FloatLit 20.0)]]
      generateBssSection stmts `shouldBe` "\tc_label: resb 1\n\tf_label: resd 1\n"

    it "extracts declarations within If statement bodies" $ do
      let stmts = [
            IfStmt (BinOp GreaterThan (Var "x") (IntLit 5))
              [DeclarationAssignment "int" "a" (IntLit 10)]  -- Then body
              [SimpleDeclaration "int" (Var "b")]]           -- Else body
      generateBssSection stmts `shouldBe` "\ta_label: resd 1\n\tb_label: resd 1\n"

    it "extracts declarations within Else statement bodies" $ do
      let stmts = [
            IfStmt (BinOp GreaterThan (Var "x") (IntLit 5))
              [SimpleDeclaration "int" (Var "a")]             -- Then body
              [DeclarationAssignment "int" "b" (IntLit 10)]]  -- Else body
      generateBssSection stmts `shouldBe` "\ta_label: resd 1\n\tb_label: resd 1\n"