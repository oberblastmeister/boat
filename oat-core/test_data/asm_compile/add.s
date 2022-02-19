	.text
	.global	program
program:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$24, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	$1, %rdi
	movq	$2, %rsi
	callq	add_two
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rdi
	callq	print_int
	movq	$0, %rax
	addq	$24, %rsp
	popq	%rbp
	retq	
	.text
	.global	add_two
add_two:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$24, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	addq	-8(%rbp), %rax
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rax
	addq	$24, %rsp
	popq	%rbp
	retq	