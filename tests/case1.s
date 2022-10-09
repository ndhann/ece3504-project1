addi  $t1, $zero, 1
add   $t0, $t1, $zero
addiu $t0, $t0, 32
addu  $t2, $t0, $t1
and   $t3, $t0, $t2
andi  $t4, $t3, 69
beq   $zero, $zero, firstlabel

secondlabel:
	sub  $t1, $t2, $t0
	subu $t1, $t1, $t0
	addi $s0, $t1, 0

firstlabel:
	bne $s0, $t1, secondlabel

	sw $s0, 0($zero)
	j  0x0000
