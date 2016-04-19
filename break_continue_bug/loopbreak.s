	.section	.rodata
.L13:

	.string "j:%d k:%d\n"
	.text

	.globl main
main:
	enter	$(200), $0
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
	movq	$15, %r10

	movq	%r10, -40(%rbp)
.L1:
	movq	-32(%rbp), %r10

	movq	-40(%rbp), %r11

	cmpq	%r11, %r10

	movq	$1, %r10

	movq	$0, %r11

	cmovlq	%r10, %r11

	movq	%r11, -48(%rbp)
	cmpq	$1, -48(%rbp)

	jne	.L2
	movq	-16(%rbp), %r10

	movq	%r10, -64(%rbp)
	movq	$2, %r10

	movq	%r10, -72(%rbp)
	movq	-64(%rbp), %r10

	movq	-72(%rbp), %r11

	cmpq	%r10, %r11

	movq	$1, %r10

	movq	$0, %r11

	cmoveq	%r10, %r11

	movq	%r11, -56(%rbp)
.L3:
	cmpq	$1, -56(%rbp)

	jne	.L4
	jmp	.L1
.L4:
	movq	-16(%rbp), %r10

	movq	%r10, -88(%rbp)
	movq	$4, %r10

	movq	%r10, -96(%rbp)
	movq	-88(%rbp), %r10

	movq	-96(%rbp), %r11

	cmpq	%r10, %r11

	movq	$1, %r10

	movq	$0, %r11

	cmoveq	%r10, %r11

	movq	%r11, -80(%rbp)
.L5:
	cmpq	$1, -80(%rbp)

	jne	.L6
	jmp	.L2
.L6:
	movq	$0, %r10

	movq	%r10, -104(%rbp)
	movq	-104(%rbp), %r10

	movq	%r10, -24(%rbp)
	movq	$19, %r10

	movq	%r10, -112(%rbp)
.L7:
	movq	-104(%rbp), %r10

	movq	-112(%rbp), %r11

	cmpq	%r11, %r10

	movq	$1, %r10

	movq	$0, %r11

	cmovlq	%r10, %r11

	movq	%r11, -120(%rbp)
	cmpq	$1, -120(%rbp)

	jne	.L8
	movq	-24(%rbp), %r10

	movq	%r10, -136(%rbp)
	movq	$1, %r10

	movq	%r10, -144(%rbp)
	movq	-136(%rbp), %r10

	movq	-144(%rbp), %r11

	cmpq	%r10, %r11

	movq	$1, %r10

	movq	$0, %r11

	cmoveq	%r10, %r11

	movq	%r11, -128(%rbp)
.L9:
	cmpq	$1, -128(%rbp)

	jne	.L10
	jmp	.L7
.L10:
	movq	-24(%rbp), %r10

	movq	%r10, -160(%rbp)
	movq	$9, %r10

	movq	%r10, -168(%rbp)
	movq	-160(%rbp), %r10

	movq	-168(%rbp), %r11

	cmpq	%r10, %r11

	movq	$1, %r10

	movq	$0, %r11

	cmoveq	%r10, %r11

	movq	%r11, -152(%rbp)
.L11:
	cmpq	$1, -152(%rbp)

	jne	.L12
	jmp	.L8
.L12:
	movq	-16(%rbp), %r10

	movq	%r10, -176(%rbp)
	movq	-24(%rbp), %r10

	movq	%r10, -184(%rbp)
	movq	-184(%rbp), %r10

	movq	%r10, %rdx

	movq	-176(%rbp), %r10

	movq	%r10, %rsi

	movq	$.L13, %r10

	movq	%r10, %rdi

	movq	$0, %rax

	call	printf
	movq	$1, %r10

	movq	%r10, -192(%rbp)
	movq	-104(%rbp), %r10

	movq	-192(%rbp), %r11

	addq	%r10, %r11

	movq	%r11, -104(%rbp)
	movq	-104(%rbp), %r10

	movq	%r10, -24(%rbp)
	jmp	.L7
.L8:
	movq	$1, %r10

	movq	%r10, -200(%rbp)
	movq	-32(%rbp), %r10

	movq	-200(%rbp), %r11

	addq	%r10, %r11

	movq	%r11, -32(%rbp)
	movq	-32(%rbp), %r10

	movq	%r10, -16(%rbp)
	jmp	.L1
.L2:
	leave

	ret
