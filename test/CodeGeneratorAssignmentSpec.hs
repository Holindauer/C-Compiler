module CodeGeneratorAssignmentSpec (main, spec) where

import Test.Hspec
import AST
import CodeGenerator

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Code Generator Literal and Variable Assignment Subroutine Creation" $ do

    -- int literal assignment
    it "Integer literal assignment subroutine generation" $ do
      
      -- setup test
      let lValue = "x"
      let index = 1
      let assignSrName = lValue ++ "_assignment_" ++ show index
      let intLitExpr = IntLit 42
      let (call, definition) = genAssignmentSr assignSrName index lValue intLitExpr
      
      -- ensure the subroutine call is correct
      call `shouldBe` "\tcall x_assignment_1\n"

      -- ensure the subroutine definition is correct
      definition `shouldBe` unlines [
        
        -- subroutine chain head
        "x_assignment_1:",
        "\tcall x_expr_eval_1_0",
        "\tmov rax, [rbp + x_label]",
        "\tmov [x_label], rax",
        "\tret",

        -- subroutine chain below
        "x_expr_eval_1_0:",
        "\tmov rax, 42",
        "\tret"]


    -- char literal assignment
    it "Character literal assignment subroutine generation" $ do
      
      -- setup test
      let lValue = "y"
      let index = 2
      let charLitExpr = CharLit 'a'
      let assignSrName = lValue ++ "_assignment_" ++ show index
      let (call, definition) = genAssignmentSr assignSrName index lValue charLitExpr

      -- ensure the call is correct
      call `shouldBe` "\tcall y_assignment_2\n"

      -- ensure the definition is correct
      definition `shouldBe` unlines [

        -- subroutine chain head
        "y_assignment_2:",
        "\tcall y_expr_eval_2_0",
        "\tmov rax, [rbp + y_label]",
        "\tmov [y_label], rax",
        "\tret",

        -- subroutine chain below
        "y_expr_eval_2_0:",
        "\tmov rax, 'a'",
        "\tret"]

    -- float literal assignment
    it "Float literal assignment subroutine generation" $ do
      
      -- setup test
      let lValue = "z"
      let index = 3
      let floatLitExpr = FloatLit 3.14
      let assignSrName = lValue ++ "_assignment_" ++ show index
      let (call, definition) = genAssignmentSr assignSrName index lValue floatLitExpr

      -- ensure the call is correct
      call `shouldBe` "\tcall z_assignment_3\n"

      -- ensure the definition is correct
      definition `shouldBe` unlines [
        "z_assignment_3:",
        "\tcall z_expr_eval_3_0",
        "\tmov rax, [rbp + z_label]",
        "\tmov [z_label], rax",
        "\tret",

        "z_expr_eval_3_0:",
        "\tmov rax, 3.14",
        "\tret"]

    -- double literal assignment
    it "Double literal assignment subroutine generation" $ do

      -- setup test
      let lValue = "w"
      let index = 4
      let doubleLitExpr = DoubleLit 3.14159
      let assignSrName = lValue ++ "_assignment_" ++ show index
      let (call, definition) = genAssignmentSr assignSrName index lValue doubleLitExpr

      -- ensure the call is correct
      call `shouldBe` "\tcall w_assignment_4\n"

      -- ensure the definition is correct
      definition `shouldBe` unlines [
        
        -- subroutine chain head
        "w_assignment_4:",
        "\tcall w_expr_eval_4_0",
        "\tmov rax, [rbp + w_label]",
        "\tmov [w_label], rax",
        "\tret",

        -- subroutine chain below
        "w_expr_eval_4_0:",
        "\tmov rax, 3.14159",
        "\tret"]

    -- variable assignment
    it "Variable assignment subroutine generation" $ do
      
      -- setup test
      let lValue = "v"
      let index = 5
      let varExpr = Var "x"
      let assignSrName = lValue ++ "_assignment_" ++ show index
      let (call, definition) = genAssignmentSr assignSrName index lValue varExpr

      -- ensure the call is correct
      call `shouldBe` "\tcall v_assignment_5\n"

      -- ensure the definition is correct
      definition `shouldBe` unlines [
        
        -- subroutine chain head
        "v_assignment_5:",
        "\tcall v_expr_eval_5_0",
        "\tmov rax, [rbp + v_label]",
        "\tmov [v_label], rax",
        "\tret",

        -- subroutine chain below
        "v_expr_eval_5_0:",
        "\tmov rax, [x_label]",
        "\tret"]

  describe "Complex Expression Assignment Subroutine Creation" $ do

    -- unary negation operator assignment
    it "Unary operator negation of w/ literal argument" $ do
      
      -- seutp test
      let lValue = "z"
      let index = 2
      let expr = UnaryOp Neg (IntLit 5)  -- unary op for negation of a literal
      let assignSrName = lValue ++ "_assignment_" ++ show index
      let (call, definition) = genAssignmentSr assignSrName index lValue expr
      
      -- ensure the call is correct
      call `shouldBe` "\tcall z_assignment_2\n"
      
      definition `shouldBe` unlines [

        -- subroutine chain head
        "z_assignment_2:",
        "\tcall z_expr_eval_2_0",      -- call evaluation subroutine for expression
        "\tmov rax, [rbp + z_label]",  -- move the result to the destination
        "\tmov [z_label], rax",        -- store the result in the appropriate variable
        "\tret",

        -- expr evaluation subroutine chain
        "z_expr_eval_2_0:",
        "\tcall z_expr_eval_2_1", -- call the evaluation subroutine for the unary operator
        "\tneg rax",              -- negate the result stored in rax        
        "\tret",

        -- literal evaluation subroutine
        "z_expr_eval_2_1:",
        "\tmov rax, 5", -- move the literal value to rax
        "\tret" ]

    -- binary operator assignment
    it "Binary operator addition of w/ two literal arguments" $ do

      -- setup test      
      let lValue = "z"
      let index = 1
      let expr = BinOp Add (IntLit 5) (IntLit 3) -- binary op for addtion of two liters
      let assignSrName = lValue ++ "_assignment_" ++ show index
      let (call, definition) = genAssignmentSr assignSrName index lValue expr
      
      
      -- ensure the call is correct
      call `shouldBe` "\tcall z_assignment_1\n"
      
      definition `shouldBe` unlines [
        
        -- defintitions of head of subroutine chain
        
        -- head of subroutine chain
        "z_assignment_1:",
        "\tcall z_expr_eval_1_0",     -- expression evaluation subroutine
        "\tmov rax, [rbp + z_label]", -- move the result to the destination
        "\tmov [z_label], rax",       -- store the result in the appropriate variable
        "\tret",
        
        -- lhs evaluation subroutine
        "z_expr_eval_1_lhs_eval_0:",
        "\tmov rax, 5",
        "\tret",

        -- rhs evaluation subroutine
        "z_expr_eval_1_rhs_eval_0:",
        "\tmov rax, 3", 
        "\tret",

        -- binOp expression evaluation subroutine chain
        "z_expr_eval_1_0:",    
        "\tcall z_expr_eval_1_lhs_eval_0",  -- call the evaluation subroutine for the lhs
        "\tpush rax",                       -- push the result of the lhs onto the stack
        "\tcall z_expr_eval_1_rhs_eval_0",  -- call the evaluation subroutine for the rhs
        "\tmov rbx, rax",                   -- move the result of the rhs to rbx
        "\tpop rax",                        -- pop the result of the lhs from the stack
        "\tadd rax, rbx",                   -- add the results of the lhs and rhs
        "\tret"
        ]


    -- binary operator assignment w/ nested expressions
    it "Code Generator Expression Assignment for Binary Op w/ nested Expressions" $ do

      -- setup test 
      let lValue = "z"
      let index = 1
      let expr = BinOp Add (UnaryOp Increment (IntLit 2)) (BinOp Subtract (IntLit 3) (IntLit 1))  -- binary op for addtion of two liters
      let assignSrName = lValue ++ "_assignment_" ++ show index
      let (call, definition) = genAssignmentSr assignSrName index lValue expr
      
      -- ensure the call is correct
      call `shouldBe` "\tcall z_assignment_1\n"
      
      definition `shouldBe` unlines [
        
        -- head of subroutine chain
        "z_assignment_1:",
        "\tcall z_expr_eval_1_0",     -- call the expression evaluation subroutine
        "\tmov rax, [rbp + z_label]", -- move the result to the destination
        "\tmov [z_label], rax",       -- store the result in the appropriate variable
        "\tret",
        
        -- unary operator evaluation subroutine
        "z_expr_eval_1_lhs_eval_0:",
        "\tcall z_expr_eval_1_lhs_eval_1",
        "\tinc rax",
        "\tret",

        -- literal evaluation subroutine
        "z_expr_eval_1_lhs_eval_1:",
        "\tmov rax, 2",
        "\tret",
        
        -- rhs evaluation subroutine
        "z_expr_eval_1_rhs_eval_lhs_eval_1:",
        "\tmov rax, 3",
        "\tret",
        
        -- rhs evaluation subroutine
        "z_expr_eval_1_rhs_eval_rhs_eval_1:",
        "\tmov rax, 1",
        "\tret",

        -- binOp expression evaluation subroutine chain
        "z_expr_eval_1_rhs_eval_1:",
        "\tcall z_expr_eval_1_rhs_eval_lhs_eval_1",
        "\tpush rax",
        "\tcall z_expr_eval_1_rhs_eval_rhs_eval_1",
        "\tmov rbx, rax",
        "\tpop rax",
        "\tsub rax, rbx",
        "\tret",

        -- expression evaluation subroutine chain head
        "z_expr_eval_1_0:",
        "\tcall z_expr_eval_1_lhs_eval_0",
        "\tpush rax",
        "\tcall z_expr_eval_1_rhs_eval_1",
        "\tmov rbx, rax",
        "\tpop rax",
        "\tadd rax, rbx",
        "\tret"
        ]