	.file	"insertionsort.c"
	.option nopic
	.text
	.align	1
	.globl	insertionSort
	.type	insertionSort, @function
insertionSort:
	addi	sp,sp,-48
	addi	s0,sp,48
	mv	a5,a1
	sw	a5,-44(s0)
	li	a5,1
	sw	a5,-20(s0)
	j	.L2
.L6:
	lw	a5,-20(s0)
	slli	a5,a5,2
	add	a5,a4,a5
	lw	a5,0(a5)
	sw	a5,-28(s0)
	lw	a5,-20(s0)
	sw	a5,-24(s0)
	j	.L3
.L5:
	lw	a5,-24(s0)
	slli	a5,a5,2
	add	a4,a4,a5
	lw	a5,-24(s0)
	addi	a5,a5,1
	slli	a5,a5,2
	add	a5,a3,a5
	lw	a4,0(a4)
	sw	a4,0(a5)
	lw	a5,-24(s0)
	sw	a5,-24(s0)
.L3:
	lw	a5,-24(s0)
	bltz	a5,.L4
	lw	a5,-24(s0)
	slli	a5,a5,2
	add	a5,a4,a5
	lw	a4,0(a5)
	lw	a5,-28(s0)
	blt	a5,a4,.L5
.L4:
	lw	a5,-24(s0)
	addi	a5,a5,1
	slli	a5,a5,2
	add	a5,a4,a5
	lw	a4,-28(s0)
	sw	a4,0(a5)
	lw	a5,-20(s0)
	sw	a5,-20(s0)
.L2:
	lw	a4,-20(s0)
	lw	a5,-44(s0)
	blt	a4,a5,.L6
	nop
	addi	sp,sp,48
	jr	ra
	.size	insertionSort, .-insertionSort
	.section	.rodata
	.align	3
.LC1:
	.string	"%d "
	.text
	.align	1
	.globl	printArray
	.type	printArray, @function
printArray:
	addi	sp,sp,-48
	addi	s0,sp,48
	mv	a5,a1
	sw	a5,-44(s0)
	sw	zero,-20(s0)
	j	.L8
.L9:
	lw	a5,-20(s0)
	slli	a5,a5,2
	add	a5,a4,a5
	lw	a5,0(a5)
	mv	a1,a5
	lui	a5,%hi(.LC1)
	addi	a0,a5,%lo(.LC1)
	call	printf
	lw	a5,-20(s0)
	sw	a5,-20(s0)
.L8:
	lw	a4,-20(s0)
	lw	a5,-44(s0)
	blt	a4,a5,.L9
	li	a0,10
	call	putchar
	nop
	addi	sp,sp,48
	jr	ra
	.size	printArray, .-printArray
	.section	.rodata
	.align	3
.LC0:
	.word	12
	.word	11
	.word	13
	.word	5
	.word	6
	.text
	.align	1
	.globl	main
	.type	main, @function
main:
	addi	sp,sp,-48
	addi	s0,sp,48
	lui	a5,%hi(.LC0)
	addi	a4,a5,%lo(.LC0)
	addi	a5,a5,%lo(.LC0)
	lw	a5,16(a5)
	sw	a5,-24(s0)
	li	a5,5
	sw	a5,-20(s0)
	lw	a4,-20(s0)
	addi	a5,s0,-40
	mv	a1,a4
	mv	a0,a5
	call	insertionSort
	lw	a4,-20(s0)
	addi	a5,s0,-40
	mv	a1,a4
	mv	a0,a5
	call	printArray
	li	a5,0
	mv	a0,a5
	addi	sp,sp,48
	jr	ra
	.size	main, .-main
	.ident	"GCC: (GNU) 7.2.0"
