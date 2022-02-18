	.text
	.global	program
program:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	$9, %rax
	addq	$5, %rax
	movq	%rax, -24(%rbp)
	movq	$15, %rax
	addq	-24(%rbp), %rax
	movq	%rax, -32(%rbp)
	movq	-32(%rbp), %rdi
	movq	$29, %rsi
	callq	assert_int_equal
	movq	$0, %rax
	addq	$32, %rsp
	popq	%rbp
	retq	