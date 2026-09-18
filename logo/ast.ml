(*
 * logo -- logo compiler and VM.
 * Copyright (C) 2026  University of Toulouse, France <casse@irit.fr>
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

type op =
	| OP_ADD
	| OP_SUB
	| OP_MUL
	| OP_DIV
	| OP_MOD
	| OP_POW

type comp =
	| COMP_EQ
	| COMP_NE
	| COMP_LT
	| COMP_LE
	| COMP_GT
	| COMP_GE

type command =
	|	NOP
	|	SEQ of command * command
	|	REPEAT of expr * command
	|	MAKE of int * expr
	|	PRINT of expr
	|	PRINTS of string
	|	IF of cond * command * command
	|	CALL_CMD of string * int * expr list
	|	SYSCALL of int * expr list

and expr =
	|	NONE
	|	CST of int
	|	VAR of int
	|	BINOP of op * expr * expr
	|	LOCAL_VAR of int

and cond =
	| 	NO_COND
	|	COMP of comp * expr * expr

(** Output expression.
	@param out	Output stream.
	@param expr	Expression to print. *)
let rec output_expr out expr =
	match expr with
	| NONE ->
		output_string out "none"
	| CST n ->
		fprintf out "cst(%d)" n
	| VAR x ->
		fprintf out "var(%d)" x
	| BINOP (op, e1, e2) ->
		fprintf out "BINOP(%s, " (op_to_str op);
		output_expr out e1;
		output_string out ", ";
		output_expr out e2;
		output_string out ")"
	| LOCAL_VAR x ->
		fprintf out "local_var(%d)" x

(** Convert operation to string.
	@param op	Operation to convert.
	@return		Operation as string. *)
and op_to_str op =
	match op with
	| OP_ADD -> "ADD"
	| OP_SUB -> "SUB"
	| OP_MUL -> "MUL"
	| OP_DIV -> "DIV"
	| OP_MOD -> "MOD"
	| OP_POW -> "POW"

(** Convert comparator to string.
	@param comp		Comparator to convert.
	@return			Corresponding string. *)
and comp_to_str comp =
	match comp with
	| COMP_EQ -> "EQ"
	| COMP_NE -> "NE"
	| COMP_LT -> "LT"
	| COMP_LE -> "LE"
	| COMP_GT -> "GT"
	| COMP_GE -> "GE"

(** Output the provided command.
	@param cmd	Command to print. *)
and output_command out cmd =
	match cmd with
	| NOP ->
		fprintf out "NOP\n"
	| SEQ(c1, c2)	->
		fprintf out "SEQ(\n";
		output_command out c1;
		fprintf out ",\n";
		output_command out c2;
		fprintf out ")\n"
	| REPEAT (n, cmd) ->
		fprintf out "REPEAT(\n";
		output_expr out n;
		output_string out ",\n[\n";
		output_command out cmd;
		fprintf out "])\n"
	| MAKE (n, e) ->
		fprintf out "MAKE(%d, " n;
		output_expr out e;
		output_string out ")\n"
	| PRINT e ->
		output_string out "PRINT(";
		output_expr out e;
		output_string out ")\n"
	| PRINTS s ->
		fprintf out "PRINTS(\"%s\")\n" s
	| IF(cond, cmd1, cmd2) ->
		output_string out "IF(";
		output_cond out cond;
		output_string out ",[\n";
		output_command out cmd1;
		output_string out "], [\n";
		output_command out cmd2;
		output_string out "])\n"
	| CALL_CMD (name, _, args) ->
		fprintf out "CALL_CMD(%s" name;
		List.iter
			(fun arg ->
				output_string out ", ";
				output_expr out arg;
				output_char out ',') args;
		fprintf out "\n)\n"
	| SYSCALL (cmd, args) ->
		fprintf out "SYSCALL(%d" cmd;
		List.iter
			(fun arg ->
				output_string out ", ";
				output_expr out arg;
				output_char out ',') args;
		fprintf out "\n)\n"


and output_cond out cond =
	match cond with
	| NO_COND ->
		output_string out "NO_COND"
	| COMP(comp, e1, e2) ->
		fprintf out "COMP(%s, " (comp_to_str comp);
		output_expr out e1;
		output_string out ", ";
		output_expr out e2;
		output_string out ")"

(** Print the provided command.
	@param cmd	Command to print. *)
let print_command = output_command stdout
