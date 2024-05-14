#!/bin/bash

# Check if the correct number of arguments is provided
if [ "$#" -ne 1 ]; then
  echo "Usage: $0 <assembly-file.asm>"
  exit 1
fi

# Get the input assembly file
ASM_FILE=$1

# Check if the file exists
if [ ! -f "$ASM_FILE" ]; then
  echo "Error: File '$ASM_FILE' not found!"
  exit 1
fi

# Get the file name without extension
FILE_BASE_NAME=$(basename "$ASM_FILE" .asm)

# Assemble the code
nasm -f elf64 -o "$FILE_BASE_NAME.o" "$ASM_FILE"
if [ $? -ne 0 ]; then
  echo "Error: Assembly failed!"
  exit 1
fi

# Link the object file without standard C library
ld -o "$FILE_BASE_NAME" "$FILE_BASE_NAME.o" -e _start
if [ $? -ne 0 ]; then
  echo "Error: Linking failed!"
  exit 1
fi

# Success message
echo "Compilation successful! You can run ./$FILE_BASE_NAME"
