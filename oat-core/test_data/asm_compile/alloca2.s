	.text
	.global	program
program:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$56, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	$17, -8(%rbp)
	movq	-8(%rbp), %r11
	movq	%r11, -40(%rbp)
	movq	-40(%rbp), %r11
	movq	%r11, -16(%rbp)
	movq	-16(%rbp), %r11
	movq	%r11, -48(%rbp)
	movq	-48(%rbp), %r11
	movq	%r11, -56(%rbp)
	movq	-56(%rbp), %rdi
	movq	$17, %rsi
	callq	assert_int_equal
	movq	$0, %rax
	addq	$56, %rsp
	popq	%rbp
	retq	