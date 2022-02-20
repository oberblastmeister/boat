	.text
	.global	program
program:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$120, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	$5, -8(%rbp)
	movq	$1, -16(%rbp)
	movq	$1, %rax
	addq	$0, %rax
	movq	%rax, -40(%rbp)
	jmp	guard
	.text
guard:
	movq	-16(%rbp), %r11
	movq	%r11, -48(%rbp)
	movq	-48(%rbp), %rax
	cmpq	$10, %rax
	setle	-56(%rbp)
	andq	$1, -56(%rbp)
	movq	$4, %rax
	addq	-40(%rbp), %rax
	movq	%rax, -64(%rbp)
	cmpq	$0, -56(%rbp)
	jne	body
	jmp	end
	.text
body:
	movq	-16(%rbp), %r11
	movq	%r11, -72(%rbp)
	movq	-8(%rbp), %r11
	movq	%r11, -80(%rbp)
	movq	$6, %rax
	addq	-64(%rbp), %rax
	movq	%rax, -88(%rbp)
	movq	-80(%rbp), %rax
	imulq	-72(%rbp), %rax
	movq	%rax, -96(%rbp)
	movq	-96(%rbp), %r11
	movq	%r11, -8(%rbp)
	movq	-16(%rbp), %r11
	movq	%r11, -104(%rbp)
	movq	$1, %rax
	addq	-104(%rbp), %rax
	movq	%rax, -112(%rbp)
	movq	-112(%rbp), %r11
	movq	%r11, -16(%rbp)
	jmp	guard
	.text
end:
	movq	-8(%rbp), %r11
	movq	%r11, -120(%rbp)
	movq	-120(%rbp), %rdi
	callq	print_int
	movq	$0, %rax
	addq	$120, %rsp
	popq	%rbp
	retq	