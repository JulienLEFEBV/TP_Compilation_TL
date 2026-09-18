@
@ Exercice 3
@
@ écrivez un programme qui affiche les 10 premiers entiers, entre 0 et 9,
@ ainsi que leur somme.
@

_start:
	reserve 2	@ 0 -> i, 1 -> s
	push #0
	set_glob 0
	push #0
	set_glob 1
loop:
	get_glob 0
	invoke 5
	get_glob 0
	get_glob 1
	add
	set_glob 1
	get_glob 0
	push #1
	add
	set_glob 0
	push #10
	get_glob 0
	goto_ge end_loop
	goto loop
end_loop:
	get_glob 1
	invoke 5
	stop
