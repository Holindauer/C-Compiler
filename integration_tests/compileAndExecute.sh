#!/bin/bash

# This script accepts a C source file and an expected exit code as arguments. It will
# compile the C code in $1 into assembly code, assemble, link, and execute the program
# using nasm and ld. Then expected exit code is compared against the actual exit code
# 0 is exited with if the actual exit code matches the expected exit code, 1 otherwise.
# NOTE: exit codes must be ints 0-255

# Check if the correct number of arguments is provided
if [ $# -ne 2 ]; then
    echo "Usage: $0 <program_name.c> <expected_exit_code>"
    exit 1
fi

# Extract the file name without the .c extension for other uses
filename="${1%.*}"
expected_exit_code=$2

# Compile to assembly
echo -e "Compiling C code to assembly code"
stack run $1 $filename.asm

# Assemble
echo -e "Assembling assembly code"
nasm -f elf64 -o $filename.o $filename.asm

# Link
echo -e "Linking object file"
ld -o $filename $filename.o

# Execute 
echo -e "Executing program"
./$filename

# Capture the actual exit code
actual_exit_code=$?

# Clean-up: delete object and executable files if they exist
echo -e "Cleaning up..."
rm -f $filename.o $filename $filename.asm

echo
echo -e "Expected exit code: $expected_exit_code"
echo "Actual exit code: $actual_exit_code"
echo

# Check the exit code against the expected exit code and return 0 or 1 depending
if [ $actual_exit_code -eq $expected_exit_code ]; then
    echo -e "Test passed for $1." && exit 0
else
    echo -e "Test failed for $1." && exit 1
fi
