@
@ Exercice 5
@
@ réalisez une boucle itérant 10 fois. A chaque itération, un nombre
@ aléatoire dans l’intervalle [0, 10] est tiré. Si le nombre est
@ inférieur à 5, la tortue tourne à droite de 30°; sinon elle tourne à
@ gauche de 30°. Dans tous les cas, elle avance ensuite de 20 unités.
@

_start:
	reserve 1	@ 0 -> i
	push #0
	set_glob 0
loop:
	push #30
	push #5
	push #11
	invoke 8
	goto_lt if
	invoke 4
	goto end_if
if:
	invoke 3
end_if:
	push #20
	invoke 1
	get_glob 0
	push #1
	add
	set_glob 0
	push #10
	get_glob 0
	goto_ge end_loop
	goto loop
end_loop:
	stop
