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

open Printf

exception LexerError of string
exception SyntaxError of string

(** Hash module for strings. *)
module StringHash = struct
	type t = string
	let equal s1 s2 = s1 = s2
	let hash s = Hashtbl.hash s
end

(** Hash table using strings as keys *)
module StringHashtbl = Hashtbl.Make(StringHash)

(** Change the suffix of a file name.
	@param name		File change to change suffix of.
	@param old_suff	Old suffix to replace.
	@param new_suff	Suffix to replace with.
	@return			File name with suffix changed. *)
let set_suffix name old_suff new_suff =
	let nl = String.length name in
	let sl = String.length old_suff in
	if sl >= nl then name ^ new_suff else
	(String.sub name 0 (nl - sl)) ^ new_suff


(** Get the line containing the given offset.
	@param path		Path of the source file.
	@param offset	Offset to select the content.
	@return			Corresponding line. *)
let line_at path offset =
	let file = open_in path in
	let rec next n sum =
		try
			let size = String.length (input_line file) + 1 in
			if sum + size > offset then (n, offset - sum + 1)
			else next (n + 1) (sum + size)
		with End_of_file ->
			(n, 0) in
	next 1 0


(** Print an error in the source.
	@param lexbuf	Current lexbuf.
	@param src		Source to look in.
	@param msg		Error message. *)
let source_error lexbuf src msg =
	let (line, col) = line_at src (Lexing.lexeme_start lexbuf) in
	printf "ERROR:%d:%d: %s\n" line col msg

(** Print an error in the source and stop the compiler.
	@param lexbuf	Current lexbuf.
	@param src		Source to look in.
	@param msg		Error message. *)
let source_fatal lexbuf prog msg =
	source_error lexbuf prog msg;
	exit 1

(** Raise a fatal error which message is displayed
	and the application stops.
	@param msg	Message to display. *)
let fatal_error msg =
	fprintf stderr "ERROR: %s\n" msg;
	exit 1

(** Record verbosity management. *)
let verbose = ref false

(** Manage verbose mode. If no verbosity, just return x. Else return f x.
	This lets f to display something about x.
	@param x	Returned value.
	@param f	Function to display something about x. *)
let say x f =
	if !verbose then f x;
	x
