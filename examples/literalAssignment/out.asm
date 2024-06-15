fromList [("d",DoubleType),("b",CharType),("j",IntType),("e",CharType),("a",IntType),("c",FloatType)]
[DeclarationAssignment "int" "a" (IntLit 10),DeclarationAssignment "float" "c" (DoubleLit 10.6),DeclarationAssignment "double" "d" (DoubleLit 10.6),DeclarationAssignment "char" "e" (Var "a"),SimpleDeclaration "char" (Var "b"),DeclarationAssignment "double" "e" (BinOp Multiply (DoubleLit 10.1) (DoubleLit 8.2)),DeclarationAssignment "int" "j" (Var "a")]
Data Declarations: [DeclarationAssignment "int" "a" (IntLit 10),DeclarationAssignment "float" "c" (DoubleLit 10.6),DeclarationAssignment "double" "d" (DoubleLit 10.6)]
Bss Declarations: [DeclarationAssignment "char" "e" (Var "a"),SimpleDeclaration "char" (Var "b"),DeclarationAssignment "double" "e" (BinOp Multiply (DoubleLit 10.1) (DoubleLit 8.2)),DeclarationAssignment "int" "j" (Var "a")]



section .data
	a dd 10	; 32-bit int
	c dq 10.6	; 64-bit double-precision float
	d dq 10.6	; 64-bit double-precision float
