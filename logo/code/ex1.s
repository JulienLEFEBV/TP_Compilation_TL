@
@ Exercice 1
@
@ Saisissez le programme.
@

_start:
	reserve 1
	push #0
	set_glob 0
L1:
	push #3
	get_glob 0
	goto_ge L2
	push #1
	get_glob 0
	add
	set_glob 0
	goto L1
L2:
	stop
