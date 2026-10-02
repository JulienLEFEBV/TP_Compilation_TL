
_start:
	debug "main"
L0:
	reserve 2
	push #10
	set_glob 0
	get_glob 0
	set_glob 1
	stop
