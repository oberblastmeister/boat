	.text
	.global	program
program:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$240, %rsp
	movq	%rdi, -16(%rbp)
	movq	%rsi, -24(%rbp)
	movq	$0, %rax
	addq	$1, %rax
	movq	%rax, -32(%rbp)
	movq	-32(%rbp), %rax
	addq	$2, %rax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	addq	$3, %rax
	movq	%rax, -48(%rbp)
	movq	-48(%rbp), %rax
	addq	$4, %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	addq	$5, %rax
	movq	%rax, -64(%rbp)
	movq	-64(%rbp), %rax
	addq	$6, %rax
	movq	%rax, -72(%rbp)
	movq	-72(%rbp), %rax
	addq	$7, %rax
	movq	%rax, -80(%rbp)
	movq	-80(%rbp), %rax
	addq	$8, %rax
	movq	%rax, -88(%rbp)
	movq	-88(%rbp), %rax
	addq	$9, %rax
	movq	%rax, -96(%rbp)
	movq	-96(%rbp), %rax
	addq	$10, %rax
	movq	%rax, -104(%rbp)
	movq	-104(%rbp), %rax
	addq	$11, %rax
	movq	%rax, -112(%rbp)
	movq	-112(%rbp), %rax
	addq	$12, %rax
	movq	%rax, -120(%rbp)
	movq	-120(%rbp), %rax
	addq	$13, %rax
	movq	%rax, -128(%rbp)
	movq	-128(%rbp), %r11
	movq	%r11, -8(%rbp)
	movq	-8(%rbp), %r11
	movq	%r11, -136(%rbp)
	movq	-136(%rbp), %rax
	addq	$14, %rax
	movq	%rax, -144(%rbp)
	movq	-144(%rbp), %rax
	addq	$15, %rax
	movq	%rax, -152(%rbp)
	movq	-152(%rbp), %rax
	addq	$16, %rax
	movq	%rax, -160(%rbp)
	movq	-160(%rbp), %rax
	addq	$17, %rax
	movq	%rax, -168(%rbp)
	movq	-168(%rbp), %rax
	addq	$18, %rax
	movq	%rax, -176(%rbp)
	movq	-176(%rbp), %rax
	addq	$19, %rax
	movq	%rax, -184(%rbp)
	movq	-184(%rbp), %rax
	addq	$20, %rax
	movq	%rax, -192(%rbp)
	movq	-192(%rbp), %rax
	addq	$21, %rax
	movq	%rax, -200(%rbp)
	movq	-200(%rbp), %rax
	addq	$22, %rax
	movq	%rax, -208(%rbp)
	movq	-208(%rbp), %rax
	addq	$23, %rax
	movq	%rax, -216(%rbp)
	movq	-216(%rbp), %rax
	addq	$24, %rax
	movq	%rax, -224(%rbp)
	movq	-224(%rbp), %rax
	addq	$25, %rax
	movq	%rax, -232(%rbp)
	movq	-232(%rbp), %rax
	addq	$26, %rax
	movq	%rax, -240(%rbp)
	movq	-240(%rbp), %rdi
	callq	print_int
	movq	$0, %rax
	addq	$240, %rsp
	popq	%rbp
	retq	