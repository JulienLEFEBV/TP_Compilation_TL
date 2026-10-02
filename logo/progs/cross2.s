
_start:
	debug "main"
L0:
	reserve 10
	push #0
	set_glob 0
	push #20
	set_glob 1
	invoke 7
	get_glob 0
	invoke 3
	get_glob 1
	invoke 1
	push #90
	get_glob 0
	add
	set_glob 2
	push #2
	get_glob 1
	mul
	set_glob 3
	invoke 7
	get_glob 2
	invoke 3
	get_glob 3
	invoke 1
	push #90
	get_glob 2
	add
	set_glob 4
	push #20
	get_glob 3
	sub
	set_glob 5
	invoke 7
	get_glob 4
	invoke 3
	get_glob 5
	invoke 1
	push #90
	get_glob 4
	add
	set_glob 6
	push #2
	get_glob 5
	mul
	set_glob 7
	invoke 7
	get_glob 6
	invoke 3
	get_glob 7
	invoke 1
	push #90
	get_glob 6
	add
	set_glob 8
	push #20
	get_glob 7
	sub
	set_glob 9
	stop
