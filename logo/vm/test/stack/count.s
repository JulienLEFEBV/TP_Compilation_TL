
@ 0: i
_start:
	reserve 1
	push #0
	set 0

loop:
	push #10
	get 0
	goto_ge end

	push #1
	get 0
	add
	set 0
	goto loop

end:
	stop


