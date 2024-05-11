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
      let intLitExpr = IntLit 42
      let (call, definition) = genExprAssignmentSr 1 "x" intLitExpr

      call `shouldBe` "\tcall x_assignment_1\n"

      definition `shouldBe` unlines [
        "x_assignment_1:",
        "\tcall x_expr_eval_1_0",
        "\tmov rax, [rbp + x_label]",
        "\tmov [x_label], rax",
        "\tret",
        "x_expr_eval_1_lit_Int_42_0:",
        "\tmov rax, 42",
        "\tret"]


    -- char literal assignment
    it "Character literal assignment subroutine generation" $ do
      let charLitExpr = CharLit 'a'
      let (call, definition) = genExprAssignmentSr 2 "y" charLitExpr

      call `shouldBe` "\tcall y_assignment_2\n"

      definition `shouldBe` unlines [
        "y_assignment_2:",
        "\tcall y_expr_eval_2_0",
        "\tmov rax, [rbp + y_label]",
        "\tmov [y_label], rax",
        "\tret",
        "y_expr_eval_2_lit_Char_'a'_0:",
        "\tmov rax, 'a'",
        "\tret"]

    -- float literal assignment
    it "Float literal assignment subroutine generation" $ do
      let floatLitExpr = FloatLit 3.14
      let (call, definition) = genExprAssignmentSr 3 "z" floatLitExpr

      call `shouldBe` "\tcall z_assignment_3\n"

      definition `shouldBe` unlines [
        "z_assignment_3:",
        "\tcall z_expr_eval_3_0",
        "\tmov rax, [rbp + z_label]",
        "\tmov [z_label], rax",
        "\tret",
        "z_expr_eval_3_lit_Float_3.14_0:",
        "\tmov rax, 3.14",
        "\tret"]

    -- double literal assignment
    it "Double literal assignment subroutine generation" $ do
      let doubleLitExpr = DoubleLit 3.14159
      let (call, definition) = genExprAssignmentSr 4 "w" doubleLitExpr

      call `shouldBe` "\tcall w_assignment_4\n"

      definition `shouldBe` unlines [
        "w_assignment_4:",
        "\tcall w_expr_eval_4_0",
        "\tmov rax, [rbp + w_label]",
        "\tmov [w_label], rax",
        "\tret",
        "w_expr_eval_4_lit_Double_3.14159_0:",
        "\tmov rax, 3.14159",
        "\tret"]

    -- variable assignment
    it "Variable assignment subroutine generation" $ do
      let varExpr = Var "x"
      let (call, definition) = genExprAssignmentSr 5 "v" varExpr

      call `shouldBe` "\tcall v_assignment_5\n"

      definition `shouldBe` unlines [
        "v_assignment_5:",
        "\tcall v_expr_eval_5_0",
        "\tmov rax, [rbp + v_label]",
        "\tmov [v_label], rax",
        "\tret",
        "v_expr_eval_5_var_x_0:",
        "\tmov rax, [x_label]",
        "\tret"]

  describe "Complex Expression Assignment Subroutine Creation" $ do

    -- unary negation operator assignment
    it "Unary operator negation of w/ literal argument" $ do

      -- unary op for negation of a literal
      let expr = UnaryOp Neg (IntLit 5)

      -- generate the subroutine
      let (call, definition) = genExprAssignmentSr 2 "z" expr
      
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
        "\tneg rax",    -- negate the result stored in rax
        "\tret",

        -- literal evaluation subroutine
        "z_expr_eval_2_lit_Int_5_1:",
        "\tmov rax, 5", -- move the literal value to rax
        "\tret" ]

    -- binary operator assignment
    it "Binary operator addition of w/ two literal arguments" $ do

      -- binary op for addtion of two liters
      let expr = BinOp Add (IntLit 5) (IntLit 3)

      -- generate the subroutine
      let (call, definition) = genExprAssignmentSr 1 "z" expr
      
      -- ensure the call is correct
      call `shouldBe` "\tcall z_assignment_1\n"
      
      definition `shouldBe` unlines [
        
        -- defintitions of head of subroutine chain
        "z_assignment_1:",
        "\tcall z_expr_eval_1_0",     -- expression evaluation subroutine
        "\tmov rax, [rbp + z_label]", -- move the result to the destination
        "\tmov [z_label], rax",
        "\tret",

        -- subroutine chain below: 

        -- evaluation of literl expr 5
        "z_expr_eval_1_lit_Int_5_1:",
        "\tmov rax, 5", 
        "\tret",

        -- evaluation of literal expr 3
        "z_expr_eval_1_lit_Int_3_2:",
        "\tmov rax, 3",
        "\tret", 

        -- evaluation of the binary op within the expression
        "z_expr_eval_1_0:",
        "\tcall z_expr_eval_1",
        "\tpush rax",
        "\tcall z_expr_eval_1_rhs_eval_2",
        "\tmov rbx, rax",
        "\tpop rax",
        "\tadd rax, rbx",
        "\tret"
        ]


    -- binary operator assignment w/ nested expressions
    it "Code Generator Expression Assignment for Binary Op w/ nested Expressions" $ do

      -- binary op for addtion of two liters
      let expr = BinOp Add (UnaryOp Increment (IntLit 2)) (BinOp Subtract (IntLit 3) (IntLit 1))

      -- generate the subroutine
      let (call, definition) = genExprAssignmentSr 1 "z" expr
      
      -- ensure the call is correct
      call `shouldBe` "\tcall z_assignment_1\n"
      
      definition `shouldBe` unlines [
        
        -- head of subroutine chain
        "z_assignment_1:",
        "\tcall z_expr_eval_1_0",     -- expression evaluation subroutine
        "\tmov rax, [rbp + z_label]", -- move the result to the destination
        "\tmov [z_label], rax",       -- store the result in the appropriate variable
        "\tret",

        -- subroutine chain below:

        "z_expr_eval_1_1:",
        "\tinc rax",
        "\tret",

        "z_expr_eval_1_lit_Int_2_2:",
        "\tmov rax, 2",
        "\tret",

        "z_expr_eval_1_lit_Int_3_4:",
        "\tmov rax, 3",
        "\tret",

        "z_expr_eval_1_lit_Int_1_5:",
        "\tmov rax, 1",
        "\tret",

        "z_expr_eval_1_3:",
        "\tcall z_expr_eval_1",
        "\tpush rax",

        "\tcall z_expr_eval_1_rhs_eval_5",
        "\tmov rbx, rax",
        "\tpop rax",
        "\tsub rax, rbx",
        "\tret",

        "z_expr_eval_1_0:",
        "\tcall z_expr_eval_1",
        "\tpush rax",
        

        "\tcall z_expr_eval_1_rhs_eval_2",
        "\tmov rbx, rax",
        "\tpop rax",
        "\tadd rax, rbx",
        "\tret"
        ]