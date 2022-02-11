	.text
	.global	program
program:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32
	movq	%rdi, 0(%rsp, %rsp)
	movq	%rsi, 8(%rsp, %rsp)
	movq	$9, %rax
	addq	$5, %rax
	movq	%rax, 24(%rsp, %rsp)
	movq	24(%rsp, %rsp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	addq	$32