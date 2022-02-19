	.text
	.global	program
program:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$88, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	$7, %rax
	addq	$7, %rax
	movq	%rax, -24(%rbp)
	jmp	l2
	.text
l2:
	movq	-24(%rbp), %rax
	imulq	$2, %rax
	movq	%rax, -32(%rbp)
	jmp	l3
	.text
l3:
	movq	$32, %rax
	subq	-32(%rbp), %rax
	movq	%rax, -40(%rbp)
	jmp	l4
	.text
l4:
	movq	$1, %rcx
	shlq	%cl, -40(%rbp)
	movq	-40(%rbp), %r11
	movq	%r11, -48(%rbp)
	jmp	l5
	.text
l5:
	movq	$60, %rcx
	shrq	%cl, -48(%rbp)
	movq	-48(%rbp), %r11
	movq	%r11, -56(%rbp)
	jmp	l6
	.text
l6:
	movq	$2, %rcx
	sarq	%cl, -56(%rbp)
	movq	-56(%rbp), %r11
	movq	%r11, -64(%rbp)
	jmp	l7
	.text
l7:
	movq	-64(%rbp), %rax
	andq	$255, %rax
	movq	%rax, -72(%rbp)
	jmp	l8
	.text
l8:
	movq	-72(%rbp), %rax
	orq	$64, %rax
	movq	%rax, -80(%rbp)
	jmp	l9
	.text
l9:
	movq	$255, %rax
	xorq	-80(%rbp), %rax
	movq	%rax, -88(%rbp)
	jmp	lexit
	.text
lexit:
	movq	-88(%rbp), %rdi
	callq	print_int
	movq	$0, %rax
	addq	$88, %rsp
	popq	%rbp
	retq	