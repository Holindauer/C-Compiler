module CodeGeneratorIncrementDecrementSpec (main, spec) where

import Test.Hspec
import AST  -- Only if needed for other parts, not used in the provided snippet
import CodeGenerator

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Code Generator Increment and Decrement Subroutine Creation" $ do

    it "Increment subroutine generation" $ do

      -- setup test
      let baseName = "increment"
      let index = 0
      let varName = "x"

      -- generate increment subroutine
      let (incCall, incDefinition) = genIncrementSr baseName index varName

      -- ensure the call to the increment subroutine is correct
      incCall `shouldBe` "\tcall increment0\n"

      -- ensure the subroutine definition is correct
      incDefinition `shouldBe` unlines [
          "increment0:",
          "\tinc [x_label]",
          "\tret"
        ]

    it "Decrement subroutine generation" $ do
        
        -- setup test
        let baseName = "decrement"
        let index = 0
        let varName = "x"
  
        -- generate decrement subroutine
        let (decCall, decDefinition) = genDecrementSr baseName index varName
  
        -- ensure the call to the decrement subroutine is correct
        decCall `shouldBe` "\tcall decrement0\n"
  
        -- ensure the subroutine definition is correct
        decDefinition `shouldBe` unlines [
            "decrement0:",
            "\tdec [x_label]",
            "\tret"
          ]