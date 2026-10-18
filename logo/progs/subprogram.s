
_start:
	debug "main"
L1:
	reserve 2
	push #50
	invoke 1
	push #90
	invoke 4
	push #30
	invoke 1
	push #10
	set_glob 0
	push #0
	set_glob 1
L2:
	get_glob 0
	get_glob 1
	goto_ge L3
	call L0
	get_glob 1
	push #1
	add
	set_glob 1
	goto L2
L3:
	stop
	debug "Function one_step"
L0:
	push #150
	invoke 3
	push #30
	invoke 1
	push #105
	invoke 4
	push #30
	invoke 1
	return_void 0
