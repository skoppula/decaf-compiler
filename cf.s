
	.section	.rodata

.L17:
	.string "ERROR: true branch is bad (2)\n"

.L22:
	.string "control flow OK if no previous output\n"

.L21:
	.string "ERROR: else branch is bad (2)\n"

	.text



	.globl main

main:

	enter	$(224), $0

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

	movq	$0, %r10
	movq	%r10, -40(%rbp)

	movq	-40(%rbp), %r10
	movq	%r10, -24(%rbp)

.L1:

	movq	$10, %r10
	movq	%r10, -48(%rbp)

	movq	-40(%rbp), %r10
	movq	-48(%rbp), %r11
	cmpq	%r11, %r10
	movq	$1, %r10
	movq	$0, %r11
	cmovlq	%r10, %r11
	movq	%r11, -56(%rbp)

	cmpq	$1, -56(%rbp)
	jne	.L3

	movq	-24(%rbp), %r10
	movq	%r10, -64(%rbp)

	movq	-16(%rbp), %r10
	movq	-64(%rbp), %r11
	addq	%r10, %r11
	movq	%r11, -16(%rbp)

	movq	$1, %r10
	movq	%r10, -72(%rbp)

	movq	-40(%rbp), %r10
	movq	-72(%rbp), %r11
	addq	%r10, %r11
	movq	%r11, -40(%rbp)

	movq	-40(%rbp), %r10
	movq	%r10, -24(%rbp)

	jmp	.L1

.L1:

	movq	$10, %r10
	movq	%r10, -48(%rbp)

	movq	-40(%rbp), %r10
	movq	-48(%rbp), %r11
	cmpq	%r11, %r10
	movq	$1, %r10
	movq	$0, %r11
	cmovlq	%r10, %r11
	movq	%r11, -56(%rbp)

	cmpq	$1, -56(%rbp)
	jne	.L3

.L3:

	movq	-16(%rbp), %r10
	movq	%r10, -88(%rbp)

	movq	$45, %r10
	movq	%r10, -96(%rbp)

	movq	-88(%rbp), %r10
	movq	-96(%rbp), %r11
	cmpq	%r10, %r11
	movq	$1, %r10
	movq	$0, %r11
	cmovneq	%r10, %r11
	movq	%r11, -80(%rbp)

.L4:

	cmpq	$1, -80(%rbp)
	jne	.L5

.L5:

	movq	$11, %r10
	movq	%r10, -104(%rbp)

	movq	-104(%rbp), %r10
	movq	%r10, -8(%rbp)

	movq	$10, %r10
	movq	%r10, -112(%rbp)

	movq	-112(%rbp), %r10
	movq	%r10, -24(%rbp)

.L7:

	movq	$0, %r10
	movq	%r10, -120(%rbp)

	movq	-112(%rbp), %r10
	movq	-120(%rbp), %r11
	cmpq	%r11, %r10
	movq	$1, %r10
	movq	$0, %r11
	cmovlq	%r10, %r11
	movq	%r11, -128(%rbp)

	cmpq	$1, -128(%rbp)
	jne	.L9

	movq	$1, %r10
	movq	%r10, -136(%rbp)

	movq	-8(%rbp), %r10
	movq	-136(%rbp), %r11
	addq	%r10, %r11
	movq	%r11, -8(%rbp)

	movq	$1, %r10
	movq	%r10, -144(%rbp)

	movq	-112(%rbp), %r10
	movq	-144(%rbp), %r11
	addq	%r10, %r11
	movq	%r11, -112(%rbp)

	movq	-112(%rbp), %r10
	movq	%r10, -24(%rbp)

	jmp	.L7

.L7:

	movq	$0, %r10
	movq	%r10, -120(%rbp)

	movq	-112(%rbp), %r10
	movq	-120(%rbp), %r11
	cmpq	%r11, %r10
	movq	$1, %r10
	movq	$0, %r11
	cmovlq	%r10, %r11
	movq	%r11, -128(%rbp)

	cmpq	$1, -128(%rbp)
	jne	.L9

.L9:

	movq	-8(%rbp), %r10
	movq	%r10, -160(%rbp)

	movq	$11, %r10
	movq	%r10, -168(%rbp)

	movq	-160(%rbp), %r10
	movq	-168(%rbp), %r11
	cmpq	%r10, %r11
	movq	$1, %r10
	movq	$0, %r11
	cmovneq	%r10, %r11
	movq	%r11, -152(%rbp)

.L10:

	cmpq	$1, -152(%rbp)
	jne	.L11

.L11:

	movq	$1, %r10
	movq	%r10, -192(%rbp)

	movq	$2, %r10
	movq	%r10, -200(%rbp)

	movq	-192(%rbp), %r10
	movq	-200(%rbp), %r11
	cmpq	%r11, %r10
	movq	$1, %r10
	movq	$0, %r11
	cmovgq	%r10, %r11
	movq	%r11, -184(%rbp)

.L14:

	cmpq	$1, -184(%rbp)
	jne	.L16

	movq	$.L17, %r10
	movq	%r10, %rdi
	movq	$0, %rax
	call	printf

	jmp	.L15

.L15:

	movq	$1, %r10
	movq	%r10, -216(%rbp)

	movq	$2, %r10
	movq	%r10, -224(%rbp)

	movq	-216(%rbp), %r10
	movq	-224(%rbp), %r11
	cmpq	%r11, %r10
	movq	$1, %r10
	movq	$0, %r11
	cmovlq	%r10, %r11
	movq	%r11, -208(%rbp)

.L18:

	cmpq	$1, -208(%rbp)
	jne	.L20

	jmp	.L19

.L19:

	movq	$.L22, %r10
	movq	%r10, %rdi
	movq	$0, %rax
	call	printf

	leave
	ret

.L20:

	movq	$.L21, %r10
	movq	%r10, %rdi
	movq	$0, %rax
	call	printf

.L16:

