	.text
	.global	program
program:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -16(%rbp)
	movq	%rsi, -24(%rbp)
	movq	$17, -8(%rbp)
	movq	-8(%rbp), %r11
	movq	%r11, -32(%rbp)
	movq	-32(%rbp), %rdi
	movq	$17, %rsi
	callq	assert_int_equal
	movq	$0, %rax
	addq	$32, %rsp
	popq	%rbp
	retq	