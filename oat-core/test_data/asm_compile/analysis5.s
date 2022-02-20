	.text
	.global	program
program:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$40, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	$2, %rax
	addq	$5, %rax
	movq	%rax, -24(%rbp)
	movq	$5, %rax
	addq	$2, %rax
	movq	%rax, -32(%rbp)
	movq	$2, %rax
	imulq	-24(%rbp)
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdi
	movq	$14, %rsi
	callq	assert_int_equal
	movq	$0, %rax
	addq	$40, %rsp
	popq	%rbp
	retq	