	.text
	.global	program
program:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$56, %rsp
	movq	%rdi, -16(%rbp)
	movq	%rsi, -24(%rbp)
	movq	$7, %rax
	imulq	$7, %rax
	movq	%rax, -32(%rbp)
	movq	-16(%rbp), %rax
	addq	$42, %rax
	movq	%rax, -40(%rbp)
	movq	$0, %rax
	cmpq	-32(%rbp), %rax
	sete	%r11b
	andq	$1, -48(%rbp)
	cmpq	$0, -48(%rbp)
	jne	l1
	jmp	l2
	.text
l1:
	movq	-8(%rbp), %r11
	movq	%r11, -56(%rbp)
	movq	-32(%rbp), %rdi
	callq	print_int
	movq	$0, %rax
	.text
l2:
	movq	$8, %rdi
	callq	print_int
	movq	$0, %rax
	addq	$56, %rsp
	popq	%rbp
	retq	