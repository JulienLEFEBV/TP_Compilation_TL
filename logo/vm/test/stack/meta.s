

.meta	0	true
.meta	1	666
.meta	2	111.666
.meta	3	"Hello, World!"
.meta	4	_end

_start:
	push	#0
	invoke 0

	push	#1
	invoke 0

	push	#2
	invoke 0

	push	#3
	invoke 0

	push	#4
	invoke 0
_end:
	stop

