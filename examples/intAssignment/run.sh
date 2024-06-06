#!/bin/bash

# Running this script will compile the C code in program.c into assembly code 
# using the compiler. Then assemble, link, and execute the assembly code using
# nasm and ld. 

# compile to assembly
echo -e "Compiling C code to assembly code" 
stack run program.c program.asm

# assemble
echo -e "Assembling assembly code" 
nasm -f elf64 -o program.o program.asm

# link
echo -e "Linking object file" 
ld -o program program.o

# execute 
echo -e "Executing program" 
./program

# delete object and executable file
# rm program.o program  # uncomment for checking return status w/ echo $?
