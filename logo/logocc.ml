(*
 * autocell - AutoCell compiler and viewer
 * Copyright (C) 2021  University of Toulouse, France <casse@irit.fr>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *)

open Common
open Stackinst
open Printf

module IntMap = Map.Make(Int)


(** Main program. *)
let _ =
	let dump_ast = ref false in
	let dump_eval = ref false in
	let one = ref false in

	let output out prog =
		Array.iteri
			(fun i x ->
				fprintf out ".meta %d %s\n" i
					(match x with
					| META_STR x -> sprintf "\"%s\"" x
					| _ -> Stackinst.meta_to_string x))
			prog.Stackinst.meta;
		fprintf out "\n_start:\n";
		Array.iter
			(fun x ->
				match x with
				| LABEL n ->
					fprintf out "L%d:\n" n
				| GOTO lab ->
					fprintf out "\tgoto L%d\n" lab
				| GOTO_EQ lab ->
					fprintf out "\tgoto_eq L%d\n" lab
				| GOTO_NE lab ->
					fprintf out "\tgoto_ne L%d\n" lab
				| GOTO_LT lab ->
					fprintf out "\tgoto_lt L%d\n" lab
				| GOTO_LE lab ->
					fprintf out "\tgoto_le L%d\n" lab
				| GOTO_GT lab ->
					fprintf out "\tgoto_gt L%d\n" lab
				| GOTO_GE lab ->
					fprintf out "\tgoto_ge L%d\n" lab
				| CALL lab ->
					fprintf out "\tcall L%d\n" lab
				| _ ->
					fprintf out "\t%s\n" (Stackinst.to_string x))
			prog.Stackinst.code in

	let compile_fun (name, lab, ast) =
		[DEBUG (sprintf "Function %s" name); LABEL lab]
		@ (Comp.comp_cmd ast)
		@ [RETURN_VOID 0] in

	let compile lexbuf name =
		try
			let ast = Parser.program Lexer.token lexbuf in
			if !dump_ast then begin
				Ast.print_command ast;
				exit 0
			end
			(*else if !dump_eval then
				(fprintf stderr "AST:\n"; print_eval stmt stderr; exit 0)*)
			else
				let l0 = Comp.new_label () in
				let insts =
					let insts = Comp.comp_cmd ast in
					[DEBUG "main"; LABEL l0]
					@ (if !Comp.var_cnt > 0 then [RESERVE !Comp.var_cnt] else [])
					@ insts
					@ [STOP]
					@ (List.flatten (List.map compile_fun !Comp.fun_list)) in
				Stackinst.make 0
					(Array.of_list insts)
					(List.length insts)
					Comp.meta_arr
					!Comp.meta_cnt
		with
		| LexerError msg ->
			source_fatal lexbuf name msg
		| Parsing.Parse_error ->
			source_fatal lexbuf name "syntax error"
		| SyntaxError msg ->
			source_fatal lexbuf name msg in

	let ignore_case_lexbuf in_chan =
		Lexing.from_function
			(fun buf len ->
				let n = input in_chan buf 0 len in
				for i = 0 to n - 1 do
					Bytes.set buf i
						(Char.lowercase_ascii (Bytes.get buf i))
				done; n) in

	(* Compile a source file *)
	let compile_path path =
		one := true;
		let prog =
			try
				let in_channel = open_in path in
				let prog = compile (ignore_case_lexbuf in_channel) path in
				close_in in_channel;
				prog
			with Sys_error msg ->
				fatal_error (sprintf "ERROR: cannot load %s: %s\n" path msg) in
		let out_path = set_suffix path ".logo" ".s" in
		try
			let out_channel = open_out out_path in
			output out_channel prog;
			fprintf stderr "Assembly saved to %s\n" out_path;
			close_out out_channel
		with Sys_error msg ->
			fprintf stderr "ERROR: cannot save %s: %s\n" out_path msg in

	(* process command line *)
	Arg.parse
		[
			("-ast", Set dump_ast, "Dump the AST.");
			("-eval", Set dump_eval, "")
		]
		compile_path
		"autocc [<file1>.auto...]";

	if not !one then
		output stdout (compile (ignore_case_lexbuf stdin) "<stdin>")

