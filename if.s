
	.section	.rodata

.L3:
	.string "ERROR: for loop is bad (1)\n"

.L4:
	.string "if OK if no previous output\n"

	.text



	.globl main

main:

	enter	$(56), $0

	movq	$0, %r10
	movq	%r10, -24(%rbp)

	movq	$0, %r10
	movq	%r10, -16(%rbp)

	movq	$0, %r10
	movq	%r10, -8(%rbp)

	movq	$0, %r10
	movq	%r10, -32(%rbp)

	movq	-32(%rbp), %r10
	movq	%r10, -16(%rbp)

	movq	-16(%rbp), %r10
	movq	%r10, -48(%rbp)

	movq	$0, %r10
	movq	%r10, -56(%rbp)

	movq	-48(%rbp), %r10
	movq	-56(%rbp), %r11
	cmpq	%r10, %r11
	movq	$1, %r10
	movq	$0, %r11
	cmovneq	%r10, %r11
	movq	%r11, -40(%rbp)

.L1:

	cmpq	$1, -40(%rbp)
	jne	.L2

	movq	$.L3, %r10
	movq	%r10, %rdi
	movq	$0, %rax
	call	printf

.L2:

	movq	$.L4, %r10
	movq	%r10, %rdi
	movq	$0, %rax
	call	printf

	leave
	ret

