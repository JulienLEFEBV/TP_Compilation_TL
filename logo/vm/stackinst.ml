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

open Printf

exception Error of string

type inst =
		| ADD
		| SUB
		| MUL
		| DIV
		| MOD
		| POW
		| PUSH of int
		| GET of int
		| SET of int
		| GOTO of int
		| GOTO_EQ of int
		| GOTO_NE of int
		| GOTO_LT of int
		| GOTO_LE of int
		| GOTO_GT of int
		| GOTO_GE of int
		| INVOKE of int
		| CALL of int
		| RESERVE of int
		| RETURN_VOID of int
		| RETURN of int
		| STOP
		| LABEL of int
		| DEBUG of string
		| GET_GLOB of int
		| SET_GLOB of int

type meta =
	| META_NONE
	| META_BOOL of bool
	| META_INT of int
	| META_FLOAT of float
	| META_STR of string


(** Type of programs in quads (initial PC, number of globals, instruction array). *)
type prog_t = {
	start: int;			(* start PC *)
	code: inst array;	(* array of instruction (maybe partially filled). *)
	meta: meta array;	(* array of meta-information. *)
}


(** Convert an instruction to a string.
	@param inst	Instruction to convert.
	@return		Resulting string. *)
let to_string inst =
	match inst with
	| ADD			-> "add"
	| SUB			-> "sub"
	| MUL			-> "mul"
	| DIV			-> "div"
	| MOD			-> "mod"
	| POW			-> "pow"
	| PUSH k		-> sprintf "push #%d" k
	| GET i			-> sprintf "get %d" i
	| SET i			-> sprintf "set %d" i
	| GOTO l		-> sprintf "goto @%04d" l
	| GOTO_EQ l		-> sprintf "goto_eq @%04d" l
	| GOTO_NE l		-> sprintf "goto_ne @%04d" l
	| GOTO_LT l		-> sprintf "goto_lt @%04d" l
	| GOTO_LE l		-> sprintf "goto_le @%04d" l
	| GOTO_GT l		-> sprintf "goto_gt @%04d" l
	| GOTO_GE l		-> sprintf "goto_ge @%04d" l
	| INVOKE c		-> sprintf "invoke %d" c
	| CALL l		-> sprintf "call @%d" l
	| RESERVE n		-> sprintf "reserve %d" n
	| RETURN_VOID n	-> sprintf "return_void %d" n
	| RETURN n		-> sprintf "return %d" n
	| STOP			-> "stop"
	| LABEL l		-> sprintf "L%d:" l
	| DEBUG s		-> sprintf "debug \"%s\"" s
	| GET_GLOB i	-> sprintf "get_glob %d" i
	| SET_GLOB i	-> sprintf "set_glob %d" i


(** Print an instruction on the given output.
	@param out	Output to print to.
	@param inst	Instruction to display. *)
let output out inst =
	fprintf out "%s\n" (to_string inst)


(** Print an instruction to the standard output.
	@param inst	Instruction to display. *)
let print inst = output stdout


(** Convert meta value to string.
	@param x	Meta-value to print.
	@return		Corresponding string.*)
let meta_to_string x =
	match x with
	| META_NONE			-> "none"
	| META_BOOL true	-> "true"
	| META_BOOL false	-> "false"
	| META_INT x		-> sprintf "%d" x
	| META_FLOAT x		-> sprintf "%f" x
	| META_STR x		-> x


(** Print a program on the given output.
	@param out	Output to print to.
	@param prog	Program to output. *)
let output_prog out prog =
	fprintf out "\t.start %d\n" prog.start;

	fprintf out "\n";
	Array.iteri (fun i meta ->
			fprintf out ".meta %d %s\n" i (meta_to_string meta)
		) prog.meta;

	fprintf out "\n";
	Array.iteri (fun i inst ->
			fprintf out "%d: %s\n" i (to_string prog.code.(i))
		) prog.code


(** Print a program on the standard output.
	@param prog	Program to output. *)
let print_prog = output_prog stdout

(** Null prorgram. *)
let null = {  start = 0; code = [| |]; meta = [| |]}

(** Build a program.
	@param start	Initial PC.
	@param code		Array of instructions.
	@param size		Size of the program.
	@param meta		Meta-information.
	@param msize	Meta-size.
	@return			Built program.*)
let make start code size meta msize =
	{
		start = start;
		code = Array.sub code 0 size;
		meta = Array.sub meta 0 msize;
	}

(** Get meta-data.
	@param prog	Program to get meta-data from.
	@param num	Number of meta-data.
	@return		Matching meta-data. *)
let get_meta prog num =
	prog.meta.(num)

(** Load a program.
	@param path		Path of the program to load.
	@return			Loaded program.
	@raise Error	If there is an error. *)
let load path =
	let error msg = raise (Error msg) in
	try
		let inp = open_in path in
		let (p: prog_t) = Marshal.from_channel inp in
		close_in inp;
		p
	with
	| Failure msg ->
		error (sprintf "cannot load the program '%s': %s" path msg)
	| Sys_error msg ->
		error (sprintf "reading program: %s" msg)


(** Save a program.
	@param prog		Program to save.
	@param path		Path to save to.
	@raise Error	If there is an error. *)
let save (prog: prog_t) path =
	try
		let out = open_out path in
		Marshal.to_channel out prog [];
		close_out out
	with Sys_error msg ->
		raise (Error msg)


(** Get the instruction at provided address.
	@param prog		Current Program.
	@return			Corresponding instruction. *)
let get_inst prog addr =
	prog.code.(addr)


(** Get the size of the program.
	@param prog		Current state.
	@return			Program size. *)
let get_inst_count prog =
	Array.length prog.code


(** Get instructions of the program.
	@param prog		Program.
	@return			Array of instructions. *)
let get_insts prog =
	prog.code
