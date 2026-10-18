
_start:
	debug "main"
L0:
	reserve 2
	push #4
	set_glob 0
	push #0
	set_glob 1
L1:
	get_glob 0
	get_glob 1
	goto_ge L2
	push #50
	invoke 1
	push #90
	invoke 3
	get_glob 1
	push #1
	add
	set_glob 1
	goto L1
L2:
	stop
