section .data

	one_float dd 1.0

	one_double dq 1.0


section .bss
	a_label: resq 1

global _start
section .text

_start:
	call a_assignment_0
	call return_stmt_1_return_1
	mov rax, 60 
	xor rdi, rdi
	syscall

	
increment_float:

	addss xmm0, dword [one_float]

	ret

increment_double:

	addsd xmm1, qword [one_double]

	ret

decrement_float:

	subss xmm0, dword [one_float]

	ret

decrement_double:

	subsd xmm1, qword [one_double]

	ret



a_assignment_0:
	call a_assignment_0_a_expr_eval_0_0
	mov [a_label], rax
	ret
a_assignment_0_a_expr_eval_0_0:
	mov rax, 10
	ret
return_stmt_1_return_1:
	call return_stmt_1_1
	mov rdi, rax
	mov rax, 60
	syscall
return_stmt_1_1:
	mov [a_label], rax
	ret

