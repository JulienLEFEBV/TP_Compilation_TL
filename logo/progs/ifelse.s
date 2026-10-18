.meta 0 "a gauche!"

_start:
	debug "main"
L0:
	reserve 1
	push #3
	set_glob 0
	get_glob 0
	push #5
	goto_eq L1
	goto L2
L1:
	push #90
	invoke 3
	goto L3
L2:
	push #90
	invoke 4
L3:
	push #20
	invoke 1
	push #0
	invoke 6
	stop
