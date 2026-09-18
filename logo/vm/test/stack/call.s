

_start:
	push #666
	push #111
	call subprog
	invoke 1
	stop

subprog:
	get -3
	get -4
	add
	return 2


