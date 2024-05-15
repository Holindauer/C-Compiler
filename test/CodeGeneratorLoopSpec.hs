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
      loopCall `shouldBe` "\tcall loop_looper_0\n"

      -- ensure the subroutine definition is correct
      loopDefinition `shouldBe` unlines [
        
        -- Subroutine chain head
        "loop_looper_0:",
        "\tcall loop_init_stmt_0x_assignment_0",
        "\tjmp for_loop_condition_0",

        -- check condition
        "for_loop_condition_0:",
        "\tcall loop_0",
        "\tcmp rax, 0",
        "\tje for_loop_end_0",
        "\tcall loop_execute_body_0",
        "\tcall loop_update_stmt_0x_assignment_0",
        "\tjmp for_loop_condition_0",

        -- Subroutine chain tail
        "for_loop_end_0:",
        "\tret",

        -- incrementer initialization
        "loop_init_stmt_0x_assignment_0:",
        "\tcall loop_init_stmt_0x_assignment_0_x_expr_eval_0_0",
        "\tmov [x_label], rax",
        "\tret",

        "loop_init_stmt_0x_assignment_0_x_expr_eval_0_0:",
        "\tmov rax, 0",
        "\tret",
        
        -- condition evaluation
        "loop_lhs_eval_0:",
        "\tmov rax, [x_label]",
        "\tret",

        "loop_rhs_eval_0:",
        "\tmov rax, 10",
        "\tret",
        
        "loop_0:",
        "\tcall loop_lhs_eval_0",
        "\tpush rax",
        "\tcall loop_rhs_eval_0",
        "\tmov rbx, rax",
        "\tpop rax",
        "\tcmp rax, rbx",
        "\tsetl al",
        "\tmovzx rax, al",
        "\tret",
        

        -- update statement
        "loop_update_stmt_0x_assignment_0:",
        "\tcall loop_update_stmt_0x_assignment_0_x_expr_eval_0_0",
        "\tmov [x_label], rax",
        "\tret",
        
        "loop_update_stmt_0x_assignment_0_x_expr_eval_0_lhs_eval_0:",
        "\tmov rax, [x_label]",
        "\tret",
        
        "loop_update_stmt_0x_assignment_0_x_expr_eval_0_rhs_eval_0:",
        "\tmov rax, 1",
        "\tret",
        
        "loop_update_stmt_0x_assignment_0_x_expr_eval_0_0:",
        "\tcall loop_update_stmt_0x_assignment_0_x_expr_eval_0_lhs_eval_0",
        "\tpush rax",
        "\tcall loop_update_stmt_0x_assignment_0_x_expr_eval_0_rhs_eval_0",
        "\tmov rbx, rax",
        "\tpop rax",
        "\tadd rax, rbx",
        "\tret",
        
        -- body execution
        "loop_body_0y_assignment_0:",
        "\tcall loop_body_0y_assignment_0_y_expr_eval_0_0",
        "\tmov [y_label], rax",
        "\tret",
        "loop_body_0y_assignment_0_y_expr_eval_0_0:",
        "\tmov rax, [x_label]",
        "\tret",
        
        "loop_execute_body_0:",
        "\tcall loop_body_0y_assignment_0",
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
        loopCall `shouldBe` "\tcall loop_looper_0\n"

        -- ensure the subroutine definition is correct
        loopDefinition `shouldBe` unlines [
          
          -- Subroutine chain head
          "loop_looper_0:",
          "\tjmp while_loop_condition_0",
          
          -- check condition
          "while_loop_condition_0:",
          "\tcall loop_0",
          "\tcmp rax, 0",
          "\tje while_loop_end_0",
          "\tcall loop_body",
          "\tjmp while_loop_condition_0",
          
          -- end of loop
          "while_loop_end_0:",
          "\tret",
          
          -- condition evaluation
          "loop_lhs_eval_0:",
          "\tmov rax, [x_label]",
          "\tret",
          
          "loop_rhs_eval_0:",
          "\tmov rax, 10",
          "\tret",
          
          "loop_0:",
          "\tcall loop_lhs_eval_0",
          "\tpush rax",
          "\tcall loop_rhs_eval_0",
          "\tmov rbx, rax",
          "\tpop rax",
          "\tcmp rax, rbx",
          "\tsetl al",
          "\tmovzx rax, al",
          "\tret",
          
          -- body execution
          "loop_body_0_y_assignment_0:",
          "\tcall loop_body_0_y_assignment_0_y_expr_eval_0_0",
          "\tmov [y_label], rax",
          "\tret",
          
          "loop_body_0_y_assignment_0_y_expr_eval_0_0:",
          "\tmov rax, [x_label]",
          "\tret",
          
          "loop_body:",
          "\tcall loop_body_0_y_assignment_0",
          "\tret"]