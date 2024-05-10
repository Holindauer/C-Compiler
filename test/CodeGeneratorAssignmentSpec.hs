module CodeGeneratorAssignmentSpec (main, spec) where

import Test.Hspec
import AST
import CodeGenerator

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "CodeGenerator.literalAssignmentSubroutine" $ do
    it "generates correct NASM code for integer literal assignments" $ do
      let (call, definition) = genLiteralAssignmentSr 1 "x" "42"
      call `shouldBe` "\tcall x_literal_assignment_1\n"
      definition `shouldBe` unlines [
        "x_literal_assignment_1:",
        "\tmov rax, 42",
        "\tmov [x_label], rax",
        "\tret"]

    it "generates correct NASM code for character literal assignments" $ do
      let (call, definition) = genLiteralAssignmentSr 2 "y" "'a'"
      call `shouldBe` "\tcall y_literal_assignment_2\n"
      definition `shouldBe` unlines [
        "y_literal_assignment_2:",
        "\tmov rax, 'a'",
        "\tmov [y_label], rax",
        "\tret"]

  describe "CodeGenerator.variableAssignmentSubroutine" $ do
    it "generates correct NASM code for variable assignments" $ do
      let (call, definition) = genVariableAssignmentSr 3 "x" "y"
      call `shouldBe` "\tcall x_to_y_3\n"
      definition `shouldBe` unlines [
        "x_to_y_3:",
        "\tmov rax, [y_label]",
        "\tmov [x_label], rax",
        "\tret"]
