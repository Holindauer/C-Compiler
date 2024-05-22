section .bss
	variable_label: resq 1

global _start
section .text

_start:
	call variable_assignment_0
	call return_stmt_1_return_1
	mov rax, 60 
	xor rdi, rdi
	syscall

variable_assignment_0:
	call variable_assignment_0_variable_expr_eval_0_0
	mov [variable_label], rax
	ret
variable_assignment_0_variable_expr_eval_0_lhs_eval_0:
	mov rax, 10
	ret
variable_assignment_0_variable_expr_eval_0_rhs_eval_0:
	call variable_assignment_0_variable_expr_eval_0_rhs_eval_1
	neg rax
	ret
variable_assignment_0_variable_expr_eval_0_rhs_eval_1:
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
return_stmt_1_return_1:
	call return_stmt_1_1
	mov rdi, rax
	mov rax, 60
	syscall
return_stmt_1_1:
	mov rax, [variable_label]
	ret
