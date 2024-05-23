#!/bin/bash

echo "Running tests..."

c_src_files_dir="c_src_files"

source_files=(
    "assignment_literal_int.c"
    "assignment_literal_double.c"  
    "assignment_literal_float.c"   
    "assignment_literal_char.c"
    "assignment_unary_op.c" 
    "assignment_binary_op.c" 
    "assignment_nested_ops.c"
    "forLoop_simple.c"
    "conditional_if.c"
    "conditional_else_if.c"
    "conditional_else.c"
    "whileLoop_simple.c"
)

expected_exit_codes=(
    2
    0
    0
    0
    2
    6
    6
    10
    3
    2
    4
    10
)

passed_tests=()
failed_tests=()

i=0  # Initialize index for accessing expected exit codes

for file in "${source_files[@]}"
do
    echo "Running test for $file"

    ./compileAndExecute.sh "$c_src_files_dir/$file" "${expected_exit_codes[$i]}"

    if [ $? -ne 0 ]; then
        echo "Test failed for $file"
        failed_tests+=("$file")
    else 
        echo "Test passed for $file"
        passed_tests+=("$file")
    fi

    ((i++))  # Increment the index for the next iteration
done

# Print the results
for file in "${passed_tests[@]}"
do
    echo "PASS - $file"
done

for file in "${failed_tests[@]}"
do
    echo
    echo "FAIL - $file"
    echo
done
