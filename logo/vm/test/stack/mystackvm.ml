open Printf

let cPRINT = 0
let cPRINT_VAL = 1

let _ =

	let syscall state cmd _ =
		if cmd == cPRINT then
			let meta = Stackvm.pop state in
			match Stackinst.get_meta state.prog meta with
			| Stackinst.META_BOOL b 	-> printf "> %B\n" b
			| Stackinst.META_INT i 		-> printf "> %d\n" i
			| Stackinst.META_FLOAT f	-> printf "> %f\n" f
			| Stackinst.META_STR s		-> printf "> %s\n" s
			| _ 						-> printf "> nil"
		else if cmd == cPRINT_VAL then
			printf "> %d\n" (Stackvm.pop state)
		else
			raise (Stackvm.Error (sprintf "unknown command: %d" cmd)) in

	let prog = Stackinst.load Sys.argv.(1) in
	let state = Stackvm.new_state prog syscall () in

	let monitor state =
		Stackvm.print_state state; printf "\n";
		printf "\t%04d %s\n" (Stackvm.get_pc state) (Stackinst.to_string (Stackvm.current_inst state));
		state in

	Stackvm.run_monitor state monitor

