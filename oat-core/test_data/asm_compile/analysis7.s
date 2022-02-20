	.text
	.global	program
program:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$80, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	$0, %rax
	addq	$10, %rax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %r11
	movq	%r11, -8(%rbp)
	movq	$0, %rax
	addq	$1, %rax
	movq	%rax, -48(%rbp)
	movq	-48(%rbp), %r11
	movq	%r11, -16(%rbp)
	jmp	guard
	.text
guard:
	movq	-16(%rbp), %r11
	movq	%r11, -56(%rbp)
	movq	-56(%rbp), %rax
	cmpq	-40(%rbp), %rax
	setl	-64(%rbp)
	andq	$1, -64(%rbp)
	cmpq	$0, -64(%rbp)
	jne	body
	jmp	end
	.text
body:
	movq	-16(%rbp), %r11
	movq	%r11, -72(%rbp)
	movq	$2, %rax
	imulq	-72(%rbp), %rax
	movq	%rax, -80(%rbp)
	movq	-80(%rbp), %r11
	movq	%r11, -16(%rbp)
	jmp	guard
	.text
end:
	movq	-40(%rbp), %rdi
	callq	print_int
	movq	$0, %rax
	addq	$80, %rsp
	popq	%rbp
	retq	