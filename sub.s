
	.section	.rodata

.L2:
	.string "Should equal 1, is %ld\n"

.L4:
	.string "Should equal 5, is %ld\n"

	.text



	.globl main

main:

	enter	$(72), $0

	movq	$0, %r10
	movq	%r10, -8(%rbp)

	movq	$3, %r10
	movq	%r10, -24(%rbp)

	movq	$2, %r10
	movq	%r10, -32(%rbp)

	movq	-24(%rbp), %r10
	movq	-32(%rbp), %r11
	subq	%r11, %r10
	movq	%r10, -16(%rbp)

.L1:

	movq	-16(%rbp), %r10
	movq	%r10, -8(%rbp)

	movq	-8(%rbp), %r10
	movq	%r10, -40(%rbp)

	movq	-40(%rbp), %r10
	movq	%r10, %rsi
	movq	$.L2, %r10
	movq	%r10, %rdi
	movq	$0, %rax
	call	printf

	movq	-8(%rbp), %r10
	movq	%r10, -56(%rbp)

	movq	$-4, %r10
	movq	%r10, -64(%rbp)

	movq	-56(%rbp), %r10
	movq	-64(%rbp), %r11
	subq	%r11, %r10
	movq	%r10, -48(%rbp)

.L3:

	movq	-48(%rbp), %r10
	movq	%r10, -8(%rbp)

	movq	-8(%rbp), %r10
	movq	%r10, -72(%rbp)

	movq	-72(%rbp), %r10
	movq	%r10, %rsi
	movq	$.L4, %r10
	movq	%r10, %rdi
	movq	$0, %rax
	call	printf

	leave
	ret

