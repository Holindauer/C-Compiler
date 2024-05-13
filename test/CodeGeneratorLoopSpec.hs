module CodeGeneratorLoopSpec (main, spec) where

import Test.Hspec
import AST
import CodeGenerator

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Code Generator For Loop Subroutine Creation" $ do

    it "Simple For Loop" $ do

      -- setup test
      let initSrName = "initX"
      let condSrName = "checkX"
      let updateSrName = "updateX"
      let bodySrName = "bodyX"
      let index = 0
      let initialization = AssignStmt "x" (IntLit 0)  -- x = 0
      let condition = BinOp LessThan (Var "x") (IntLit 10)  -- x < 10
      let update = AssignStmt "x" (BinOp Add (Var "x") (IntLit 1))  -- x = x + 1
      let body = [AssignStmt "y" (Var "x")]  -- y = x
      let (loopCall, loopDefinition) = genForLoopSr "loop" index initialization condition update body

      -- ensure the call to the for loop subroutine is correct
      loopCall `shouldBe` "\tcall loop_for_loop_0\n"

      -- ensure the subroutine definition is correct
      loopDefinition `shouldBe` unlines [
        
        -- Subroutine chain head
        "loop_for_loop_0:",
        "\tcall loop_init_stmt_0",
        "\tjmp for_loop_condition_0",

        -- For loop condition
        "for_loop_condition_0:",
        "\tcall loop_0",               -- call loop_0
        "\tcmp rax, 0",                -- compare rax to 0
        "\tje for_loop_end_0",         -- if rax == 0, jump to for_loop_end_0
        "\tcall loop_body_0",          -- otherwise call loop_body_0
        "\tcall loop_update_stmt_0",   -- call loop_update_stmt_0
        "\tjmp for_loop_condition_0",  -- jump back to start of for loop

        -- For loop end  (just returns)
        "for_loop_end_0:",
        "\tret",

        -- initialization statement
        "loop_init_stmt_0x_assignment_0:",
        "\tcall x_expr_eval_0_0",
        "\tmov rax, [rbp + x_label]",
        "\tmov [x_label], rax",
        "\tret",
        
        "x_expr_eval_0_0:", -- expr eval for x = 0
        "\tmov rax, 0",
        "\tret",
        

        -- The following subroutines evaluate the condition of the for loop
        -- and move the result into rax
        "loop_lhs_eval_0:",
        "\tmov rax, [x_label]", -- lhs eval
        "\tret",

        "loop_rhs_eval_0:",     -- rhs eval
        "\tmov rax, 10",
        "\tret",

        "loop_0:",
        "\tcall loop_lhs_eval_0",  -- call lhs eval
        "\tpush rax",              -- push rax to stack
        "\tcall loop_rhs_eval_0",  -- call rhs eval
        "\tmov rbx, rax",          -- move rax to rbx
        "\tpop rax",               -- pop rax from stack
        "\tcmp rax, rbx",          -- compare rax to rbx
        "\tsetl al",               -- set al to 1 if rax < rbx
        "\tmovzx rax, al",         -- move al to rax
        "\tret",
        
        -- update statement
        "loop_update_stmt_0x_assignment_0:",
        "\tcall x_expr_eval_0_0",     -- call x_expr_eval_0_0 (to update the incrementer)
        "\tmov rax, [rbp + x_label]", -- move x_label to rax
        "\tmov [x_label], rax",       -- move rax to x_label
        "\tret",
        
        -- The following evaluate the expression in the update statement
        "x_expr_eval_0_lhs_eval_0:",
        "\tmov rax, [x_label]", -- move value in x_label to rax
        "\tret",
        
        "x_expr_eval_0_rhs_eval_0:",
        "\tmov rax, 1", -- move 1 to rax
        "\tret",

        "x_expr_eval_0_0:",
        "\tcall x_expr_eval_0_lhs_eval_0",  -- subroutine call to move lhs value (x_label) to rax
        "\tpush rax",                       -- push rax to stack
        "\tcall x_expr_eval_0_rhs_eval_0",  -- subroutine call to move rhs value (1) to rax
        "\tmov rbx, rax",                   -- move rax to rbx
        "\tpop rax",                        -- pop rax from stack
        "\tadd rax, rbx",                   -- add rbx to rax
        "\tret",

        -- body of the loop  (an assignment statement)
        "loop_body_0y_assignment_0:",
        "\tcall y_expr_eval_0_0",
        "\tmov rax, [rbp + y_label]",
        "\tmov [y_label], rax",
        "\tret",

        "y_expr_eval_0_0:",
        "\tmov rax, [x_label]",
        "\tret"]


    describe "Code Generator While Loop Subroutine Creation" $ do

      it "Simple While Loop" $ do

        -- setup test
        let condSrName = "checkX"
        let bodySrName = "bodyX"
        let index = 0
        let condition = BinOp LessThan (Var "x") (IntLit 10)  -- x < 10
        let body = [AssignStmt "y" (Var "x")]  -- y = x
        let (loopCall, loopDefinition) = genWhileLoopSr "loop" index condition body

        -- ensure the call to the while loop subroutine is correct
        loopCall `shouldBe` "\tcall loop_while_loop_0\n"

        -- ensure the subroutine definition is correct
        loopDefinition `shouldBe` unlines [
          
          -- Subroutine chain head
          "loop_while_loop_0:",
          "\tjmp while_loop_condition_0",  -- kickstart while loop

          -- While loop body
          "while_loop_condition_0:",
          "\tcall loop_0",                -- call loop_0
          "\tcmp rax, 0",                 -- compare rax to 0         
          "\tje while_loop_end_0",        -- if rax == 0, jump to while_loop_end_0
          "\tcall loop_body_0",           -- otherwise call loop_body_0
          "\tjmp while_loop_condition_0", -- jump back to start of while loop
          
          -- While loop end  (just returns)
          "while_loop_end_0:", 
          "\tret",

          -- evaluation of the condition of the while loop
          "loop_lhs_eval_0:",
          "\tmov rax, [x_label]",
          "\tret",
          
          "loop_rhs_eval_0:",
          "\tmov rax, 10",
          "\tret",  
          
          "loop_0:",
          "\tcall loop_lhs_eval_0",  -- call lhs eval
          "\tpush rax",              -- push rax to stack
          "\tcall loop_rhs_eval_0",  -- call rhs eval
          "\tmov rbx, rax",          -- move rax to rbx
          "\tpop rax",               -- pop rax from stack
          "\tcmp rax, rbx",          -- compare rax to rbx
          "\tsetl al",               -- set al to 1 if rax < rbx
          "\tmovzx rax, al",         -- move al to rax
          "\tret",
          
          -- body of the loop  (an assignment statement)
          "loop_body_0y_assignment_0:",
          "\tcall y_expr_eval_0_0",
          "\tmov rax, [rbp + y_label]",
          "\tmov [y_label], rax",
          "\tret",
          
          "y_expr_eval_0_0:",
          "\tmov rax, [x_label]",
          "\tret"]