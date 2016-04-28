
	.section	.rodata

.L4:
	.string "ERROR: true branch is bad (2)\n"

.L5:
	.string "control flow OK if no previous output\n"

	.text



	.globl main

main:

	enter	$(24), $0

	movq	$1, %r10
	movq	%r10, -16(%rbp)

	movq	$2, %r10
	movq	%r10, -24(%rbp)

	movq	-16(%rbp), %r10
	movq	-24(%rbp), %r11
	cmpq	%r11, %r10
	movq	$1, %r10
	movq	$0, %r11
	cmovgq	%r10, %r11
	movq	%r11, -8(%rbp)

.L1:

	cmpq	$1, -8(%rbp)
	jne	.L3

	movq	$.L4, %r10
	movq	%r10, %rdi
	movq	$0, %rax
	call	printf

	jmp	.L2

.L3:

.L2:

	movq	$.L5, %r10
	movq	%r10, %rdi
	movq	$0, %rax
	call	printf

	leave
	ret

