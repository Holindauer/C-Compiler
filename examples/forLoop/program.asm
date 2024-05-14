section .bss
	variable_label: resq 1
	i_label: resq 1

global _start
section .text

_start:
	; Call the main subroutine
	call variable_assignment_0
	call for_loop_1_for_loop_1

	; Exit the program properly
	mov rax, 60      ; syscall number for exit
	xor rdi, rdi     ; exit status 0
	syscall          ; perform the system call to exit

variable_assignment_0:
	call variable_expr_eval_0_0
	mov [variable_label], rax
	ret
variable_expr_eval_0_0:
	mov rax, 0
	ret
for_loop_1_for_loop_1:
	call for_loop_1_init_stmt_1
	jmp for_loop_condition_1
for_loop_condition_1:
	call for_loop_1_1
	cmp rax, 0
	je for_loop_end_1
	call for_loop_1_body_0
	call for_loop_1_update_stmt_1
	jmp for_loop_condition_1
for_loop_end_1:
	ret
for_loop_1_init_stmt_1i_assignment_1:
	call i_expr_eval_1_0
	mov [i_label], rax
	ret
i_expr_eval_1_0:
	mov rax, 0
	ret
for_loop_1_lhs_eval_1:
	mov rax, [i_label]
	ret
for_loop_1_rhs_eval_1:
	mov rax, 10
	ret
for_loop_1_1:
	call for_loop_1_lhs_eval_1
	push rax
	call for_loop_1_rhs_eval_1
	mov rbx, rax
	pop rax
	cmp rax, rbx
	setl al
	movzx rax, al
	ret
for_loop_1_update_stmt_1increment_11:
	inc [i_label]
	ret
for_loop_1_body_0variable_assignment_0:
	call variable_expr_eval_0_0
	mov [variable_label], rax
	ret
variable_expr_eval_0_lhs_eval_0:
	mov rax, [variable_label]
	ret
variable_expr_eval_0_rhs_eval_0:
	mov rax, 1
	ret
variable_expr_eval_0_0:
	call variable_expr_eval_0_lhs_eval_0
	push rax
	call variable_expr_eval_0_rhs_eval_0
	mov rbx, rax
	pop rax
	add rax, rbx
	ret
