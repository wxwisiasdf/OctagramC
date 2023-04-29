.text
__occ_ctor:
	pushl	%ebp
	movl	%esp,%ebp
	andl	$16,%esp
	movl	$__ms_4,%ebx
	movl	%ebx,0(%esp)
	call	$printf
	movl	%eax,%eax
	movl	$g_dstd,%eax
.data
.globl	g_dstd
	.align	4
g_dstd:
	.space	4
__ms_4:
	.ascii "êÈH\0"
