module CodeGeneratorConditionalSpec (main, spec) where

import Test.Hspec
import AST
import CodeGenerator

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Code Generator Conditional Subroutine Creation" $ do

    it "Conditional subroutine generation with simple then and else branches" $ do

      -- setup test
      let condSrName = "testCond"
      let index = 0
      let condition = Var "x"  -- Assuming Var is a constructor for Expr type representing a variable
      let thenBody = [AssignStmt "x" (IntLit 10)]  -- then-body: simple assignment
      let elseBody = []                            -- else-body: empty for now 
      let (condCall, condDefinition) = genConditionalSr condSrName index condition thenBody elseBody

      -- ensure the call to the conditional subroutine is correct
      condCall `shouldBe` "\tcall testCond_0\n"

      -- ensure the subroutine definition is correct
      condDefinition `shouldBe` unlines [
        
        -- head of the subroutine chain        
        "testCond_0:",
        "\tcall testCond_0_eval_cond_0_0", 
        "\tcmp rax, 1",                  -- compare the result of the conditional expression evaluation to 1
        "\tje testCond_0_execute_body",  -- tje == jump if equal (to 1)
        "\tret",

        -- condition eval subroutines (simple in this case)
        "testCond_0_eval_cond_0_0:",
        "\tmov rax, [x_label]", 
        "\tret",
        
        -- body of the conditional (sequential stmt sr calls in then-body)
        "testCond_0_execute_body:",
        "\tcall testCond_0_then_body_0x_assignment_0",
        "\tret",
        
        -- then-body stmt sr
        "testCond_0_then_body_0x_assignment_0:", 
        "\tcall x_expr_eval_0_0",     -- call eval of conditional expr (moves it into rax)
        "\tmov rax, [rbp + x_label]", -- 
        "\tmov [x_label], rax",
        "\tret",

        -- eval of conditional expr (simple literal in this case)
        "x_expr_eval_0_0:",
        "\tmov rax, 10",
        "\tret"]

    it "Conditiona subroutine w/ multiple statements in the body" $ do
            
        -- setup test
        let condSrName = "testCond"
        let index = 1
        let condition = Var "x"  -- Assuming Var is a constructor for Expr type representing a variable
        let thenBody = [AssignStmt "x" (IntLit 10), AssignStmt "y" (IntLit 20)]  -- then-body: multiple assignments
        let elseBody = []                            -- else-body: empty for now 
        let (condCall, condDefinition) = genConditionalSr condSrName index condition thenBody elseBody
    
        -- ensure the call to the conditional subroutine is correct
        condCall `shouldBe` "\tcall testCond_1\n"
    
        -- ensure the subroutine definition is correct
        condDefinition `shouldBe` unlines [
          
          -- head of the subroutine chain        
          "testCond_1:",
          "\tcall testCond_1_eval_cond_1_1",  -- call the conditional stmt evaluation subroutine
          "\tcmp rax, 1",                           -- compare the result of the conditional expression evaluation to 1
          "\tje testCond_1_execute_body",           -- tje == jump if equal (to 1)
          "\tret", 

          -- condition eval subroutines (simple literal in this case)
          "testCond_1_eval_cond_1_1:",
          "\tmov rax, [x_label]",
          "\tret",
        
          -- body of the conditional (sequential stmt sr calls in then-body)
          "testCond_1_execute_body:",
          "\tcall testCond_1_then_body_0x_assignment_0", -- call the first assignment in the then-body
          "\tcall testCond_1_then_body_1y_assignment_1", -- call the second assignment in the then-body
          "\tret",
          
          -- then-body stmt sr chain 1
          "testCond_1_then_body_0x_assignment_0:",
          "\tcall x_expr_eval_0_0",
          "\tmov rax, [rbp + x_label]",
          "\tmov [x_label], rax",
          "\tret",
          
          "x_expr_eval_0_0:",  
          "\tmov rax, 10",
          "\tret",
        
        -- them body stmt sr chain 2
          "testCond_1_then_body_1y_assignment_1:",
          "\tcall y_expr_eval_1_0",
          "\tmov rax, [rbp + y_label]",
          "\tmov [y_label], rax",
          "\tret",
          
          "y_expr_eval_1_0:",
          "\tmov rax, 20",
          "\tret"]
