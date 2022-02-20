	.text
	.global	program
program:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$144, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	$0, %rax
	addq	$30, %rax
	movq	%rax, -40(%rbp)
	movq	$24, %rax
	subq	-40(%rbp), %rax
	movq	%rax, -48(%rbp)
	movq	-48(%rbp), %rax
	subq	$9, %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %r11
	movq	%r11, -8(%rbp)
	movq	-8(%rbp), %r11
	movq	%r11, -64(%rbp)
	movq	$4, %rax
	imulq	-56(%rbp), %rax
	movq	%rax, -72(%rbp)
	movq	-72(%rbp), %r11
	movq	%r11, -16(%rbp)
	movq	-16(%rbp), %r11
	movq	%r11, -80(%rbp)
	movq	-72(%rbp), %rax
	cmpq	$10, %rax
	setg	-88(%rbp)
	andq	$1, -88(%rbp)
	cmpq	$0, -88(%rbp)
	jne	then
	jmp	else
	.text
then:
	movq	-16(%rbp), %r11
	movq	%r11, -96(%rbp)
	movq	$10, %rax
	subq	-96(%rbp), %rax
	movq	%rax, -104(%rbp)
	movq	-104(%rbp), %r11
	movq	%r11, -16(%rbp)
	jmp	merge
	.text
else:
	movq	-16(%rbp), %r11
	movq	%r11, -112(%rbp)
	movq	$10, %rax
	addq	-112(%rbp), %rax
	movq	%rax, -120(%rbp)
	movq	-120(%rbp), %r11
	movq	%r11, -16(%rbp)
	jmp	merge
	.text
merge:
	movq	-16(%rbp), %r11
	movq	%r11, -128(%rbp)
	movq	-40(%rbp), %rax
	subq	$60, %rax
	movq	%rax, -136(%rbp)
	movq	-136(%rbp), %rax
	imulq	-128(%rbp), %rax
	movq	%rax, -144(%rbp)
	movq	-144(%rbp), %rdi
	callq	print_int
	movq	$0, %rax
	addq	$144, %rsp
	popq	%rbp
	retq	