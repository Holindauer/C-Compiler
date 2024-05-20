section .bss
	i_label: resq 1

global _start
section .text

_start:
	call i_assignment_0
	call while_loop_1_looper_1
	mov rax, 60 
	xor rdi, rdi
	syscall

i_assignment_0:
	call i_assignment_0_i_expr_eval_0_0
	mov [i_label], rax
	ret
i_assignment_0_i_expr_eval_0_0:
	mov rax, 0
	ret
while_loop_1_looper_1:
	jmp while_loop_condition_1
while_loop_condition_1:
	call while_loop_1_0
	cmp rax, 0
	je while_loop_end_1
	call while_loop_1_body_execute_body
	jmp while_loop_condition_1
while_loop_end_1:
	ret
while_loop_1_lhs_eval_0:
	mov rax, [i_label]
	ret
while_loop_1_rhs_eval_0:
	mov rax, 10
	ret
while_loop_1_0:
	call while_loop_1_lhs_eval_0
	push rax
	call while_loop_1_rhs_eval_0
	mov rbx, rax
	pop rax
	cmp rax, rbx
	setl al
	movzx rax, al
	ret
while_loop_1_body_execute_body:
	call while_loop_1_body_0i_assignment_0
	ret
while_loop_1_body_0i_assignment_0:
	call while_loop_1_body_0i_assignment_0_i_expr_eval_0_0
	mov [i_label], rax
	ret
while_loop_1_body_0i_assignment_0_i_expr_eval_0_lhs_eval_0:
	mov rax, [i_label]
	ret
while_loop_1_body_0i_assignment_0_i_expr_eval_0_rhs_eval_0:
	mov rax, 1
	ret
while_loop_1_body_0i_assignment_0_i_expr_eval_0_0:
	call while_loop_1_body_0i_assignment_0_i_expr_eval_0_lhs_eval_0
	push rax
	call while_loop_1_body_0i_assignment_0_i_expr_eval_0_rhs_eval_0
	mov rbx, rax
	pop rax
	add rax, rbx
	ret
