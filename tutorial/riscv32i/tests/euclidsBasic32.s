	.file	"euclidsBasic.c"
	.option nopic
	.text
	.align	1
	.globl	euclidsAlgorithm
	.type	euclidsAlgorithm, @function
euclidsAlgorithm:
	addi	sp,sp,-32
	sw	ra,28(sp)
	sw	s0,24(sp)
	addi	s0,sp,32
	sw	a0,-20(s0)
	sw	a1,-24(s0)
	lw	a4,-24(s0)
	lw	a5,-20(s0)
	ble	a4,a5,.L2
	li	a5,-1
	j	.L3
.L2:
	lw	a4,-24(s0)
	lw	a5,-20(s0)
	bne	a4,a5,.L4
	lw	a5,-20(s0)
	j	.L3
.L4:
	lw	a5,-24(s0)
	bnez	a5,.L5
	lw	a5,-20(s0)
	j	.L3
.L5:
	lw	a4,-20(s0)
	lw	a5,-24(s0)
	rem	a5,a4,a5
	mv	a1,a5
	lw	a0,-24(s0)
	call	euclidsAlgorithm
	mv	a5,a0
.L3:
	mv	a0,a5
	lw	ra,28(sp)
	lw	s0,24(sp)
	addi	sp,sp,32
	jr	ra
	.size	euclidsAlgorithm, .-euclidsAlgorithm
	.align	1
	.globl	main
	.type	main, @function
main:
	addi	sp,sp,-32
	sw	ra,28(sp)
	sw	s0,24(sp)
	addi	s0,sp,32
	li	a5,255
	sw	a5,-20(s0)
	li	a5,5
	sw	a5,-24(s0)
	lw	a1,-24(s0)
	lw	a0,-20(s0)
	call	euclidsAlgorithm
	mv	a5,a0
	mv	a0,a5
	lw	ra,28(sp)
	lw	s0,24(sp)
	addi	sp,sp,32
	jr	ra
	.size	main, .-main
	.ident	"GCC: (GNU) 8.1.0"
