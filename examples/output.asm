section .bss
	a_label: resq 1
	b_label: resb 1
global _start
section .text
_start:
	call a_assignment_0

	; Exit the program properly
	mov rax, 60      ; syscall number for exit
	xor rdi, rdi     ; exit status 0
	syscall          ; perform the system call to exit

a_assignment_0:
	call a_expr_eval_0_0
	mov [a_label], rax
	ret
a_expr_eval_0_0:
	mov rax, 10
	ret
