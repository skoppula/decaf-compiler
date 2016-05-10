
	.section	.rodata

.L1:
	.string "%d\n"

.L2:
	.string "%d\n"

.L3:
	.string "%d\n"

.L4:
	.string "%d\n"

.L5:
	.string "%d\n"

.L6:
	.string "%d\n"

	.text



	.globl main

main:

	enter	$(152), $0

	movq	$2, -8(%rbp)

	movq	-8(%rbp), %r10
	movq	%r10, %r12

	movq	%r12, %r10
	movq	%r10, -64(%rbp)

	movq	-64(%rbp), %r10
	movq	%r10, -16(%rbp)

	movq	%r12, %r10
	movq	%r10, -72(%rbp)

	movq	-72(%rbp), %r10
	movq	%r10, -24(%rbp)

	movq	%r12, %r10
	movq	%r10, -80(%rbp)

	movq	-80(%rbp), %r10
	movq	%r10, -32(%rbp)

	movq	%r12, %r10
	movq	%r10, -88(%rbp)

	movq	-88(%rbp), %r10
	movq	%r10, -40(%rbp)

	movq	%r12, %r10
	movq	%r10, -96(%rbp)

	movq	-96(%rbp), %r10
	movq	%r10, -48(%rbp)

	movq	%r12, %r10
	movq	%r10, -104(%rbp)

	movq	%r12, %r10
	movq	%r10, -8(%rbp)

	movq	-104(%rbp), %r10
	movq	%r10, %rsi
	movq	$.L1, %r10
	movq	%r10, %rdi
	movq	$0, %rax
	call	printf

	movq	-16(%rbp), %r10
	movq	%r10, -112(%rbp)

	movq	-112(%rbp), %r10
	movq	%r10, %rsi
	movq	$.L2, %r10
	movq	%r10, %rdi
	movq	$0, %rax
	call	printf

	movq	-24(%rbp), %r10
	movq	%r10, -120(%rbp)

	movq	-120(%rbp), %r10
	movq	%r10, %rsi
	movq	$.L3, %r10
	movq	%r10, %rdi
	movq	$0, %rax
	call	printf

	movq	-32(%rbp), %r10
	movq	%r10, -128(%rbp)

	movq	-128(%rbp), %r10
	movq	%r10, %rsi
	movq	$.L4, %r10
	movq	%r10, %rdi
	movq	$0, %rax
	call	printf

	movq	-40(%rbp), %r10
	movq	%r10, -136(%rbp)

	movq	-136(%rbp), %r10
	movq	%r10, %rsi
	movq	$.L5, %r10
	movq	%r10, %rdi
	movq	$0, %rax
	call	printf

	movq	-48(%rbp), %r10
	movq	%r10, -144(%rbp)

	movq	-144(%rbp), %r10
	movq	%r10, %rsi
	movq	$.L6, %r10
	movq	%r10, %rdi
	movq	$0, %rax
	call	printf

	movq	$0, -152(%rbp)

	movq	-152(%rbp), %rax

	leave
	ret

