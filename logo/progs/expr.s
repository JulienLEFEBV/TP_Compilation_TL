
_start:
	debug "main"
L0:
	reserve 2
	push #3
	set_glob 0
	push #2
	set_glob 1
	push #1
	get_glob 0
	sub
	invoke 5
	push #2
	get_glob 0
	push #1
	sub
	sub
	invoke 5
	push #2
	push #1
	mul
	invoke 5
	push #3
	push #2
	push #1
	mul
	mul
	invoke 5
	push #3
	push #2
	push #1
	mul
	add
	invoke 5
	push #3
	push #2
	mul
	push #1
	add
	invoke 5
	push #3
	push #2
	push #1
	add
	mul
	invoke 5
	push #2
	push #1
	div
	invoke 5
	push #2
	push #1
	mod
	invoke 5
	push #3
	push #2
	push #1
	mul
	div
	invoke 5
	push #3
	push #2
	mod
	push #3
	div
	push #2
	push #1
	mul
	add
	invoke 5
	push #2
	push #1
	pow
	invoke 5
	push #3
	push #2
	pow
	push #1
	pow
	invoke 5
	push #3
	push #2
	pow
	push #1
	mul
	invoke 5
	push #3
	push #2
	add
	push #1
	pow
	invoke 5
	stop
