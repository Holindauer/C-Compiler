#!/bin/bash

echo "Running tests..."

# Type directories each containing the source files
type_dirs=("int") # "float" "char" "string")

# Source files that need to be tested
source_files=("literalAssignment.c" "exprAssignment.c" "if.c" "elseIf.c" "else.c" "whileLoop.c" "forLoop.c")

# Expected exit code after execution
expected_exit_code=0

test_results=()

# Loop over each type directory
for dir in "${type_dirs[@]}"; do

    # Loop over each source file within the current type directory
    for file in "${source_files[@]}"; do
        echo "Running test for $dir/$file"
        
        # Compile and execute the source file
        ./compileAndExecute.sh $dir/$file $expected_exit_code
        
        # Check the exit code of compilation
        if [ $? -ne 0 ]; then
            test_results+=("FAIL... $dir/$file")
        fi
        if [ $? -eq 0 ]; then
            test_results+=("pass... $dir/$file")
        fi

    done
done


clear
echo "Test results:"
for result in "${test_results[@]}"; do
    printf "%s\n" "$result"
done
