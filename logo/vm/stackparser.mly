/*
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
 */

%{

open Printf
open Stackinst
open Common

let insts = Array.make 1000 STOP
let cnt = ref 0
let meta = Array.make 1000 Stackinst.META_NONE
let mcnt = ref 0

(** Table of labels. *)
let labels: int StringHashtbl.t = StringHashtbl.create 253

(** Table of backpatches. *)
let backpatches: (string * (int -> unit)) list ref = ref []

(** Add a new quad to the program.
	@param inst		Added quad. *)
let emit inst =
	insts.(!cnt) <- inst;
	incr cnt

(** Get the number of the next instruction.
	@return	Next instruction number. *)
let next_inst _ =
	!cnt

(** Apply a backpatch on the goto at address g in order to make it
	branch to address a.
	@param g	Address of goto to patch.
	@param a	Branch address. *)
let backpatch g a =
	insts.(g) <-
		match insts.(g) with
		| GOTO _		-> GOTO a
		| GOTO_EQ _		-> GOTO_EQ a
		| GOTO_NE _		-> GOTO_NE a
		| GOTO_LT _		-> GOTO_LT a
		| GOTO_LE _		-> GOTO_LE a
		| GOTO_GT _		-> GOTO_GT a
		| GOTO_GE _		-> GOTO_GE a
		| CALL _		-> CALL a
		| _				-> failwith "Backpatched a non-goto instruction!"

(** Backpatch a meta-data
	@param num	Meta-number.
	@param addr	Address to resolve. *)
let backpatch_meta num addr =
	meta.(num) <- META_INT addr

(** Try to resolve a label or recod an instruction backpatch on the next
	instruction.
	@param label	Label to resolve.
	@return			Resolved address or 0. *)
let resolve_label label =
	try
		StringHashtbl.find labels label
	with Not_found -> begin
		backpatches := (label, backpatch (next_inst ())) :: !backpatches;
		0
	end

(** Resolve a label for a meta-data or record it for backpatch.
	@param label	Label to resolve.
	@param num		Meta-data number to resolve.
	@return			Resolved address or 0. *)
let resolve_meta label num =
	try
		StringHashtbl.find labels label
	with Not_found -> begin
		backpatches := (label, backpatch_meta num) :: !backpatches;
		0
	end

(** Resolve all backpatches. *)
let resolve_backpatches _ =
	let rec resolve patches =
		match patches with
		| [] -> ()
		| (l, f)::t ->
			try
				f (StringHashtbl.find labels l);
				resolve t
			with Not_found ->
				Printf.fprintf stderr "ERROR: cannot resolve %s" l;
				raise Exit in
	resolve !backpatches

(** Add a new meta-value.
	@param n	Number of the meta.
	@param x	Value of the meta. *)
let set_meta n x =
	meta.(n) <- x;
	mcnt := max !mcnt (n + 1)

%}

%token ADD
%token SUB
%token MUL
%token DIV
%token MOD
%token POW
%token PUSH
%token GET
%token SET
%token GOTO
%token GOTO_EQ
%token GOTO_NE
%token GOTO_LT
%token GOTO_LE
%token GOTO_GT
%token GOTO_GE
%token INVOKE
%token CALL
%token RESERVE
%token RETURN
%token RETURN_VOID
%token STOP
%token DEBUG
%token GET_GLOB
%token SET_GLOB

%token <bool> 	BOOL
%token <int> 	INT
%token <float> 	FLOAT
%token <string>	LABEL
%token <string> STR

%token COMMA
%token SHARP
%token COLON
%token META
%token EOF

%type <Stackinst.prog_t> listing
%start listing

%%

listing: commands EOF
	{
		emit STOP;
		resolve_backpatches ();
		let start =
			try
				say
					(StringHashtbl.find labels "_start")
					(fun start -> printf "Start at %04d\n" start)
			with Not_found ->
				fprintf stderr "WARNING: no _start label. Assuming start at 0!\n";
				0 in
		Stackinst.make start insts !cnt meta !mcnt
	}
;

commands:
	command
		{ () }
|	command commands
		{ () }
;

command:
	META INT BOOL
		{ set_meta $2 (META_BOOL $3) }
|	META INT INT
		{ set_meta $2 (META_INT $3) }
|	META INT FLOAT
		{ set_meta $2 (META_FLOAT $3) }
|	META INT STR
		{ set_meta $2 (META_STR $3) }
|	META INT LABEL
		{ set_meta $2 (META_INT (resolve_meta $3 $2)) }
|	LABEL COLON
		{
			if StringHashtbl.mem labels $1
			then raise (SyntaxError (sprintf "label %s already used!" $1))
			else StringHashtbl.add labels $1 (next_inst ())
		}
|	ADD
		{ emit ADD }
|	SUB
		{ emit SUB }
|	MUL
		{ emit MUL }
|	DIV
		{ emit DIV }
|	MOD
		{ emit MOD }
|	POW
		{ emit POW }
|	PUSH SHARP INT
		{ emit (PUSH $3) }
|	GET INT
		{ emit (GET $2) }
|	SET INT
		{ emit (SET $2) }
|	GOTO LABEL
		{ emit (GOTO (resolve_label $2)) }
|	GOTO_EQ LABEL
		{ emit (GOTO_EQ (resolve_label $2)) }
|	GOTO_NE LABEL
		{ emit (GOTO_NE (resolve_label $2)) }
|	GOTO_LT LABEL
		{ emit (GOTO_LT (resolve_label $2)) }
|	GOTO_LE LABEL
		{ emit (GOTO_LE (resolve_label $2)) }
|	GOTO_GT LABEL
		{ emit (GOTO_GT (resolve_label $2)) }
|	GOTO_GE LABEL
		{ emit (GOTO_GE (resolve_label $2)) }
|	INVOKE INT
		{ emit (INVOKE $2) }
|	CALL LABEL
		{ emit (CALL (resolve_label $2)) }
|	RESERVE INT
		{ emit (RESERVE $2) }
|	RETURN
		{ emit (RETURN 0) }
|	RETURN INT
		{ emit (RETURN $2) }
|	RETURN_VOID
		{ emit (RETURN_VOID 0) }
|	RETURN_VOID INT
		{ emit (RETURN_VOID $2) }
|	STOP
		{ emit STOP }
|	DEBUG STR
		{ emit (DEBUG $2) }
|	GET_GLOB INT
		{ emit (GET_GLOB $2) }
|	SET_GLOB INT
		{ emit (SET_GLOB $2) }
;

%%

