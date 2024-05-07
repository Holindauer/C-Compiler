module CodeGenerateBssSpec (main, spec) where

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
      generateBssSection stmts `shouldBe` "x_label: resd 1\n"

    it "generates correct .bss section for multiple declarations" $ do
      let stmts = [SimpleDeclaration "int" (Var "x"), SimpleDeclaration "double" (Var "y")]
      generateBssSection stmts `shouldBe` "x_label: resd 1\ny_label: resq 1\n"

    it "handles declarations with initial values" $ do
      let stmts = [DeclarationAssignment "float" "z" (IntLit 0)]
      generateBssSection stmts `shouldBe` "z_label: resd 1\n"

    it "ignores non-declaration statements" $ do
      let stmts = [ExprStmt (IntLit 5), SimpleDeclaration "char" (Var "c")]
      generateBssSection stmts `shouldBe` "c_label: resb 1\n"
    
    describe "generateBssSection with multiple variable declarations interspersed throughout statement list" $ do

      it "generates correct .bss section for multiple declarations" $ do
        let stmts = [SimpleDeclaration "int" (Var "x"), ExprStmt (IntLit 5), SimpleDeclaration "double" (Var "y")]
        generateBssSection stmts `shouldBe` "x_label: resd 1\ny_label: resq 1\n"

      it "handles declarations with initial values" $ do
        let stmts = [SimpleDeclaration "int" (Var "x"), DeclarationAssignment "float" "z" (IntLit 0)]
        generateBssSection stmts `shouldBe` "x_label: resd 1\nz_label: resd 1\n"

      it "ignores non-declaration statements" $ do
        let stmts = [SimpleDeclaration "int" (Var "x"), ExprStmt (IntLit 5), SimpleDeclaration "char" (Var "c")]
        generateBssSection stmts `shouldBe` "x_label: resd 1\nc_label: resb 1\n"

    describe "generate with generateCode to include .bss header" $ do 

        it "generates full assembly code with .bss section" $ do
            let program = [SimpleDeclaration "int" (Var "x"), SimpleDeclaration "double" (Var "y")]
            let expected = "section .bss\nx_label: resd 1\ny_label: resq 1\nsection .text\n"
            generateCode program `shouldBe` expected
