
_start:
	push #5
	call fact
	invoke 1
	stop

fact:
	push #1
	get -3
	goto_le	end

	push #1
	get	-3
	sub
	call fact

	get -3
	mul

	return 1

end:
	push	#1
	return 1





