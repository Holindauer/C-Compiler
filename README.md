# C-Compiler

This repository implements a compiler for a subset of C that was written in Haskell. C source code is compiled into x86-64 assembly code of the NASM specification. 

At this stage, the specific subset of C includes assignments, for loops, while loops, and  conditional statements with some restrictions on syntax. Accepted data types include int, float, char, and double. Pointers, arrays, and libraries are not supported. Compilation is also limited to a single main function with no additional functions or global variables.

The purpose of this implementation is thus mostly on the educational side for learning how compilation works under the hood. 


# How to Use

First, you will need to install haskell/stack, ld, and nasm if not already:

    sudo apt install haskel-platform nasm binutils

Within this directory place a C source file that adheres to the compilation restrictions and run the following command. Replace the fields with your programs name and the desired name for the assembly file that will be produced.

    stack run [your_program.c] [assembly_output.asm]

This will produced an assembly file. To make this into a binary, run the following commands. This will assemble, link, and execute the produced x86-64. Note that the folllowing argument names can be changed.

    nasm -f elf64 -o object_file.o assembly_output.asm
    ld -o binary_file object_file.o  
    ./binary_file     


# How it works

The compiler has 3 basic compoents:
- Lexer
- Parser 
- Code Generator

### Lexer
When a source file enters the compiler, it is first passed to the lexer, which will extract all unique words of the language into a list of tokens. 

For example, if this is the program string :
 
    "int main(void){ int variable = 1 + 2; return 0; }"

It will extract the following tokens:

    [TInt,TMain,TLparen,TVoid,TRparen,TLbrace,TInt,TIdent "variable",TAssign,TIntLit 1,TPlus,TIntLit 2,TSemicolon,TReturn,TIntLit 0,TSemicolon,TRbrace,TEOF]

It should be noted that at this stage, there is not much useful info here as to what order these statements should be executed. This is less obvious in this example, but consider a nested loop. 

In order to determine the order of execution for the entire program, the gramatical structure of the language must be parsed from these tokens.

### Parser

It is the job of the parser to determine and package each statement within the list of tokens into a format that details the type of statement and the order of execution for each component. This is a recursive process that involves populating an Abstract Syntax Tree (AST) with each nested expression/statement. 

In this implementation, assignments, declarations, loops, and conditionals are all consdiered statements. For the tokens above, the following statements AST is produced:

    [DeclarationAssignment "int" "variable" (BinOp Add (IntLit 1) (IntLit 2))],[]

Note that the binary op is nested within the DeclarationAssignment AST. 

Each statement at the 0'th level of indentation for the program will be parsed into a seperate AST and collected into a list.

### Code Generation

Once the list of ASTs are collected, nasm x86-64 assembly code is generated. 

The code generator will first scan through each AST to determine all the allocated memory. This is used to created a .bss section, which allocates space for variables within the program. 

Then, for each statement collected will be created a psuedo-recursive chain of subroutines that evaluates the entire AST in a depth first manner (almost like an explicitly defined unfolded recursive function). 

The entry point for the program is defined in the _start module and contains sequential calls the head of each subroutine chain. 

This in turn executes the program.

Here is the assmebly produced for that example:

    section .bss
        variable_label: resq 1

    global _start
    section .text

    _start:
        ; Call the main subroutine
        call variable_assignment_0

        ; Exit the program properly
        mov rax, 60      ; syscall number for exit
        xor rdi, rdi     ; exit status 0
        syscall          ; perform the system call to exit

    variable_assignment_0:
        call variable_assignment_0_variable_expr_eval_0_0
        mov [variable_label], rax
        ret
    variable_assignment_0_variable_expr_eval_0_lhs_eval_0:
        mov rax, 1
        ret
    variable_assignment_0_variable_expr_eval_0_rhs_eval_0:
        mov rax, 2
        ret
    variable_assignment_0_variable_expr_eval_0_0:
        call variable_assignment_0_variable_expr_eval_0_lhs_eval_0
        push rax
        call variable_assignment_0_variable_expr_eval_0_rhs_eval_0
        mov rbx, rax
        pop rax
        add rax, rbx
        ret


# Sytnax Restrictions

The following syntax restrictions are enforced by the lexer and parser:

- Data types are limited to int, char, float, and double.
- Programs are limited to what can be fit within the main function. Compilation of functions is not yet implemented.
- A program must start with "int main(void){" and must end with "return 0;}"
