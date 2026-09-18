(*
 * VM -- virtual machine library
 * Copyright (C) 2025  University of Toulouse, France <casse@irit.fr>
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
open Printf

let source = ref ""
let target = ref ""

let opts = [
	("-o", Arg.Set_string target, "Use the given file as output.");
	("-v", Arg.Set verbose, "Enable verbose mode.")
]
let  doc = "Stack machine assembler."

let free_arg s =
	if !source <> ""
	then raise (Arg.Bad "Too many source files specified.")
	else source := s

(** Display something in verbose mode. *)
let say x f =
	if !verbose then f x else x

let _ =
	Arg.parse opts free_arg doc;
	let path, file =
		if !source = ""
		then ("<stdin>", stdin)
		else (!source, open_in !source) in
	let lexbuf = Lexing.from_channel file in
	try
		let prog = Stackparser.listing Stacklexer.scan lexbuf in

		(* compute output path *)
		let out_path =
			if !target <> "" then !target
			else if !source = "" then "a.out"
			else set_suffix !source ".s" ".exe" in

		(* Save the program *)
		Stackinst.save prog out_path;
		fprintf stderr "Saved to %s!\n" out_path

	with
	|	Parsing.Parse_error ->
			source_fatal lexbuf path "syntax error"
	|	Common.LexerError msg ->
			source_fatal lexbuf path msg
	|	Common.SyntaxError msg ->
			source_fatal lexbuf path msg
	|	Stackinst.Error msg ->
			fatal_error msg
