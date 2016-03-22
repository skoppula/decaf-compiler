	.comm	field_in_class,8,8
	.section	.rodata
.L1:

	.string "args: %d+%d+%d+%d+%d+%d=%d\n"
.L17:

	.string "short circuit failed in and\n"
.L18:

	.string "short circuit failed in or\n"
.L19:

	.string "hello world from main\n"
.L20:

	.string "some values (13,14): %d %d\n"
.L21:

	.string "returned a variable, value is %d, should be 81\n"
.L22:

	.string "returned a constant, value is %d, should be 1\n"
.L26:

	.string "successfully did a short-circuited and.\n"
.L27:

	.string "if failed on a && proc\n"
.L32:

	.string "if failed on !a && proc\n"
.L33:

	.string "successfully did a short-circuited and.\n"
.L37:

	.string "successfully did a short-circuited or.\n"
.L38:

	.string "if failed on a || proc\n"
.L43:

	.string "successfully did a short-circuited or.\n"
.L44:

	.string "if failed on !a || proc\n"
.L48:

	.string "sum from 0 to 99 is %d, should be 4950\n"
	.text

test_long_args:
	enter	$(272), $0

	movq	%r9, %r10

	movq	%r10, -48(%rbp)

	movq	%r8, %r10

	movq	%r10, -40(%rbp)

	movq	%rcx, %r10

	movq	%r10, -32(%rbp)

	movq	%rdx, %r10

	movq	%r10, -24(%rbp)

	movq	%rsi, %r10

	movq	%r10, -16(%rbp)

	movq	%rdi, %r10

	movq	%r10, -8(%rbp)
	movq	-8(%rbp), %r10

	movq	%r10, -56(%rbp)
	movq	-16(%rbp), %r10

	movq	%r10, -64(%rbp)
	movq	-24(%rbp), %r10

	movq	%r10, -72(%rbp)
	movq	-32(%rbp), %r10

	movq	%r10, -80(%rbp)
	movq	-40(%rbp), %r10

	movq	%r10, -88(%rbp)
	movq	-48(%rbp), %r10

	movq	%r10, -96(%rbp)
	movq	-8(%rbp), %r10

	movq	%r10, -144(%rbp)
	movq	-16(%rbp), %r10

	movq	%r10, -152(%rbp)
	movq	-144(%rbp), %r10

	movq	-152(%rbp), %r11

	addq	%r10, %r11

	movq	%r11, -136(%rbp)
.L2:
	movq	-24(%rbp), %r10

	movq	%r10, -160(%rbp)
	movq	-136(%rbp), %r10

	movq	-160(%rbp), %r11

	addq	%r10, %r11

	movq	%r11, -128(%rbp)
.L3:
	movq	-32(%rbp), %r10

	movq	%r10, -168(%rbp)
	movq	-128(%rbp), %r10

	movq	-168(%rbp), %r11

	addq	%r10, %r11

	movq	%r11, -120(%rbp)
.L4:
	movq	-40(%rbp), %r10

	movq	%r10, -176(%rbp)
	movq	-120(%rbp), %r10

	movq	-176(%rbp), %r11

	addq	%r10, %r11

	movq	%r11, -112(%rbp)
.L5:
	movq	-48(%rbp), %r10

	movq	%r10, -184(%rbp)
	movq	-112(%rbp), %r10

	movq	-184(%rbp), %r11

	addq	%r10, %r11

	movq	%r11, -104(%rbp)
.L6:
	subq	$16, %rsp

	movq	-104(%rbp), %r10

	movq	%r10, 8(%rsp)

	movq	-96(%rbp), %r10

	movq	%r10, (%rsp)

	movq	-88(%rbp), %r10

	movq	%r10, %r9

	movq	-80(%rbp), %r10

	movq	%r10, %r8

	movq	-72(%rbp), %r10

	movq	%r10, %rcx

	movq	-64(%rbp), %r10

	movq	%r10, %rdx

	movq	-56(%rbp), %r10

	movq	%r10, %rsi

	movq	$.L1, %r10

	movq	%r10, %rdi

	movq	$0, %rax

	call	printf

	addq	$16, %rsp
	movq	-8(%rbp), %r10

	movq	%r10, -232(%rbp)
	movq	-16(%rbp), %r10

	movq	%r10, -240(%rbp)
	movq	-232(%rbp), %r10

	movq	-240(%rbp), %r11

	addq	%r10, %r11

	movq	%r11, -224(%rbp)
.L7:
	movq	-24(%rbp), %r10

	movq	%r10, -248(%rbp)
	movq	-224(%rbp), %r10

	movq	-248(%rbp), %r11

	addq	%r10, %r11

	movq	%r11, -216(%rbp)
.L8:
	movq	-32(%rbp), %r10

	movq	%r10, -256(%rbp)
	movq	-216(%rbp), %r10

	movq	-256(%rbp), %r11

	addq	%r10, %r11

	movq	%r11, -208(%rbp)
.L9:
	movq	-40(%rbp), %r10

	movq	%r10, -264(%rbp)
	movq	-208(%rbp), %r10

	movq	-264(%rbp), %r11

	addq	%r10, %r11

	movq	%r11, -200(%rbp)
.L10:
	movq	-48(%rbp), %r10

	movq	%r10, -272(%rbp)
	movq	-200(%rbp), %r10

	movq	-272(%rbp), %r11

	addq	%r10, %r11

	movq	%r11, -192(%rbp)
.L11:
	movq	-192(%rbp), %rax
	leave

	ret
	movq	$-2, %rdi

	call	exit

test_short_circuit:
	enter	$(72), $0

	movq	%rsi, %r10

	movq	%r10, -16(%rbp)

	movq	%rdi, %r10

	movq	%r10, -8(%rbp)
	movq	-16(%rbp), %r10

	movq	%r10, -32(%rbp)
	movq	$1, -40(%rbp)
	movq	-32(%rbp), %r10

	movq	-40(%rbp), %r11

	cmpq	%r10, %r11

	movq	$1, %r10

	movq	$0, %r11

	cmovneq	%r10, %r11

	movq	%r11, -24(%rbp)
.L12:
	cmpq	$1, -24(%rbp)

	jne	.L13
	movq	-8(%rbp), %r10

	movq	%r10, -56(%rbp)
	movq	$1, -64(%rbp)
	movq	-56(%rbp), %r10

	movq	-64(%rbp), %r11

	cmpq	%r10, %r11

	movq	$1, %r10

	movq	$0, %r11

	cmovneq	%r10, %r11

	movq	%r11, -48(%rbp)
.L14:
	cmpq	$1, -48(%rbp)

	jne	.L16
	movq	$.L17, %r10

	movq	%r10, %rdi

	movq	$0, %rax

	call	printf
	jmp	.L15
.L16:
	movq	$.L18, %r10

	movq	%r10, %rdi

	movq	$0, %rax

	call	printf
.L15:
.L13:
	movq	$1, -72(%rbp)
	movq	-72(%rbp), %rax
	leave

	ret
	movq	$-2, %rdi

	call	exit

	.globl main
main:
	enter	$(576), $0
	movq	$.L19, %r10

	movq	%r10, %rdi

	movq	$0, %rax

	call	printf
	movq	$13, %r10

	movq	%r10, -48(%rbp)
	movq	-48(%rbp), %r10

	movq	%r10, field_in_class(%rip)
	movq	$14, %r10

	movq	%r10, -56(%rbp)
	movq	-56(%rbp), %r10

	movq	%r10, -8(%rbp)
	movq	field_in_class(%rip), %r10

	movq	%r10, -64(%rbp)
	movq	-8(%rbp), %r10

	movq	%r10, -72(%rbp)
	movq	-72(%rbp), %r10

	movq	%r10, %rdx

	movq	-64(%rbp), %r10

	movq	%r10, %rsi

	movq	$.L20, %r10

	movq	%r10, %rdi

	movq	$0, %rax

	call	printf
	movq	field_in_class(%rip), %r10

	movq	%r10, -96(%rbp)
	movq	field_in_class(%rip), %r10

	movq	%r10, -112(%rbp)
	movq	field_in_class(%rip), %r10

	movq	%r10, -128(%rbp)
	movq	-8(%rbp), %r10

	movq	%r10, -144(%rbp)
	movq	-8(%rbp), %r10

	movq	%r10, -160(%rbp)
	movq	-8(%rbp), %r10

	movq	%r10, -176(%rbp)
	movq	-176(%rbp), %r10

	movq	%r10, %r9

	movq	-160(%rbp), %r10

	movq	%r10, %r8

	movq	-144(%rbp), %r10

	movq	%r10, %rcx

	movq	-128(%rbp), %r10

	movq	%r10, %rdx

	movq	-112(%rbp), %r10

	movq	%r10, %rsi

	movq	-96(%rbp), %r10

	movq	%r10, %rdi

	movq	$0, %rax

	call	test_long_args

	movq	%rax, -80(%rbp)
	movq	-80(%rbp), %r10

	movq	%r10, -16(%rbp)
	movq	-16(%rbp), %r10

	movq	%r10, -184(%rbp)
	movq	-184(%rbp), %r10

	movq	%r10, %rsi

	movq	$.L21, %r10

	movq	%r10, %rdi

	movq	$0, %rax

	call	printf
	movq	$0, -208(%rbp)
	movq	$1, -224(%rbp)
	movq	-224(%rbp), %r10

	movq	%r10, %rsi

	movq	-208(%rbp), %r10

	movq	%r10, %rdi

	movq	$0, %rax

	call	test_short_circuit

	movq	%rax, -192(%rbp)
	movq	-192(%rbp), %r10

	movq	%r10, -24(%rbp)
	movq	-24(%rbp), %r10

	movq	%r10, -232(%rbp)
	movq	-232(%rbp), %r10

	movq	%r10, %rsi

	movq	$.L22, %r10

	movq	%r10, %rdi

	movq	$0, %rax

	call	printf
	movq	$1, -240(%rbp)
	movq	-240(%rbp), %r10

	movq	%r10, -24(%rbp)
	movq	-24(%rbp), %r10

	movq	%r10, -256(%rbp)
	movq	-256(%rbp), %r10

	movq	%r10, -248(%rbp)
	cmpq	$1, -256(%rbp)

	jne	.L23
	movq	$1, -280(%rbp)
	movq	$1, -296(%rbp)
	movq	-296(%rbp), %r10

	movq	%r10, %rsi

	movq	-280(%rbp), %r10

	movq	%r10, %rdi

	movq	$0, %rax

	call	test_short_circuit

	movq	%rax, -264(%rbp)
	movq	-256(%rbp), %r10

	movq	-264(%rbp), %r11

	andq	%r10, %r11

	movq	%r11, -248(%rbp)
.L23:
	cmpq	$1, -248(%rbp)

	jne	.L25
	movq	$.L26, %r10

	movq	%r10, %rdi

	movq	$0, %rax

	call	printf
	jmp	.L24
.L25:
	movq	$.L27, %r10

	movq	%r10, %rdi

	movq	$0, %rax

	call	printf
.L24:
	movq	-24(%rbp), %r10

	movq	%r10, -320(%rbp)
	movq	$1, -328(%rbp)
	movq	-320(%rbp), %r10

	movq	-328(%rbp), %r11

	cmpq	%r10, %r11

	movq	$1, %r10

	movq	$0, %r11

	cmovneq	%r10, %r11

	movq	%r11, -312(%rbp)
.L28:
	movq	-312(%rbp), %r10

	movq	%r10, -304(%rbp)
	cmpq	$1, -312(%rbp)

	jne	.L29
	movq	$1, -352(%rbp)
	movq	$0, -368(%rbp)
	movq	-368(%rbp), %r10

	movq	%r10, %rsi

	movq	-352(%rbp), %r10

	movq	%r10, %rdi

	movq	$0, %rax

	call	test_short_circuit

	movq	%rax, -336(%rbp)
	movq	-312(%rbp), %r10

	movq	-336(%rbp), %r11

	andq	%r10, %r11

	movq	%r11, -304(%rbp)
.L29:
	cmpq	$1, -304(%rbp)

	jne	.L31
	movq	$.L32, %r10

	movq	%r10, %rdi

	movq	$0, %rax

	call	printf
	jmp	.L30
.L31:
	movq	$.L33, %r10

	movq	%r10, %rdi

	movq	$0, %rax

	call	printf
.L30:
	movq	-24(%rbp), %r10

	movq	%r10, -384(%rbp)
	movq	-384(%rbp), %r10

	movq	%r10, -376(%rbp)
	cmpq	$1, -384(%rbp)

	je	.L34
	movq	$0, -408(%rbp)
	movq	$0, -424(%rbp)
	movq	-424(%rbp), %r10

	movq	%r10, %rsi

	movq	-408(%rbp), %r10

	movq	%r10, %rdi

	movq	$0, %rax

	call	test_short_circuit

	movq	%rax, -392(%rbp)
	movq	-384(%rbp), %r10

	movq	-392(%rbp), %r11

	orq	%r10, %r11

	movq	%r11, -376(%rbp)
.L34:
	cmpq	$1, -376(%rbp)

	jne	.L36
	movq	$.L37, %r10

	movq	%r10, %rdi

	movq	$0, %rax

	call	printf
	jmp	.L35
.L36:
	movq	$.L38, %r10

	movq	%r10, %rdi

	movq	$0, %rax

	call	printf
.L35:
	movq	-24(%rbp), %r10

	movq	%r10, -448(%rbp)
	movq	$1, -456(%rbp)
	movq	-448(%rbp), %r10

	movq	-456(%rbp), %r11

	cmpq	%r10, %r11

	movq	$1, %r10

	movq	$0, %r11

	cmovneq	%r10, %r11

	movq	%r11, -440(%rbp)
.L39:
	movq	-440(%rbp), %r10

	movq	%r10, -432(%rbp)
	cmpq	$1, -440(%rbp)

	je	.L40
	movq	$0, -480(%rbp)
	movq	$1, -496(%rbp)
	movq	-496(%rbp), %r10

	movq	%r10, %rsi

	movq	-480(%rbp), %r10

	movq	%r10, %rdi

	movq	$0, %rax

	call	test_short_circuit

	movq	%rax, -464(%rbp)
	movq	-440(%rbp), %r10

	movq	-464(%rbp), %r11

	orq	%r10, %r11

	movq	%r11, -432(%rbp)
.L40:
	cmpq	$1, -432(%rbp)

	jne	.L42
	movq	$.L43, %r10

	movq	%r10, %rdi

	movq	$0, %rax

	call	printf
	jmp	.L41
.L42:
	movq	$.L44, %r10

	movq	%r10, %rdi

	movq	$0, %rax

	call	printf
.L41:
	movq	$0, %r10

	movq	%r10, -504(%rbp)
	movq	-504(%rbp), %r10

	movq	%r10, -16(%rbp)
	movq	$100, %r10

	movq	%r10, -512(%rbp)
	movq	-512(%rbp), %r10

	movq	%r10, -8(%rbp)
	movq	$0, %r10

	movq	%r10, -520(%rbp)
	movq	-520(%rbp), %r10

	movq	%r10, -40(%rbp)
	movq	-8(%rbp), %r10

	movq	%r10, -528(%rbp)
.L45:
	movq	-520(%rbp), %r10

	movq	-528(%rbp), %r11

	cmpq	%r11, %r10

	movq	$1, %r10

	movq	$0, %r11

	cmovlq	%r10, %r11

	movq	%r11, -536(%rbp)
	cmpq	$1, -536(%rbp)

	jne	.L46
	movq	-16(%rbp), %r10

	movq	%r10, -552(%rbp)
	movq	-40(%rbp), %r10

	movq	%r10, -560(%rbp)
	movq	-552(%rbp), %r10

	movq	-560(%rbp), %r11

	addq	%r10, %r11

	movq	%r11, -544(%rbp)
.L47:
	movq	-544(%rbp), %r10

	movq	%r10, -16(%rbp)
	movq	$1, %r10

	movq	%r10, -568(%rbp)
	movq	-520(%rbp), %r10

	movq	-568(%rbp), %r11

	addq	%r10, %r11

	movq	%r11, -520(%rbp)
	movq	-520(%rbp), %r10

	movq	%r10, -40(%rbp)
	jmp	.L45
.L46:
	movq	-16(%rbp), %r10

	movq	%r10, -576(%rbp)
	movq	-576(%rbp), %r10

	movq	%r10, %rsi

	movq	$.L48, %r10

	movq	%r10, %rdi

	movq	$0, %rax

	call	printf
	leave

	ret
