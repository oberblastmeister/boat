	.text
	.global	program
program:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	%rdi, -16(%rbp)
	movq	%rsi, -24(%rbp)
	movq	$7, %rax
	imulq	$7, %rax
	movq	%rax, -32(%rbp)
	movq	-16(%rbp), %rax
	addq	$42, %rax
	movq	%rax, -40(%rbp)
	jmp	l1
	.text
l1:
	movq	-8(%rbp), %r11
	movq	%r11, -48(%rbp)
	movq	-32(%rbp), %rdi
	callq	print_int
	movq	$0, %rax
	addq	$48, %rsp
	popq	%rbp
	retq	