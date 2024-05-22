section .bss
	a_label: resq 1
	b_label: resb 1

global _start
section .text

_start:
	call a_assignment_0
	mov rax, 60 
	xor rdi, rdi
	syscall

a_assignment_0:
	call a_assignment_0_a_expr_eval_0_0
	mov [a_label], rax
	ret
a_assignment_0_a_expr_eval_0_0:
	mov rax, 10
	ret
