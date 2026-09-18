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
open Stackinst

type 'a state_t = {
	prog: prog_t;
	mem: int array;
	mutable pc: int;
	mutable sp: int;
	mutable fp: int;
	invoke: 'a state_t -> int -> 'a -> 'a;
	mutable env: 'a;
}


(** Stack size. *)
let stack_size = 512


(** Raised if there is an error in the execution of instructions. *)
exception Error of string


(** Create a new VM state.
	@param prog		Program to execute.
	@param invoke	Invoke implementation.
	@param env		Environment value. *)
let new_state prog invoke env =
	{
		prog = prog;
		mem = Array.make stack_size 0;
		pc = prog.start;
		sp = 0;
		fp = 0;
		invoke = invoke;
		env = env;
	}


(** Pop the top value of the stack.
	@param state	State to pop from and to update.
	@return			Popped value.
	@raise Error	If the stack is empty. *)
let pop (state: 'a state_t) =
	try
		state.sp <- state.sp - 1;
		let res = state.mem.(state.sp) in
		res
	with Invalid_argument _ ->
		raise (Error "pop into empty stack!")


(** Push a value to the the stack.
	@param state	State to pop from and to update.
	@param x		Value to push.
	@raise Error	If the stack is empty. *)
let push (state: 'a state_t) x =
	try
		state.mem.(state.sp) <- x;
		state.sp <- state.sp + 1
	with Invalid_argument _ ->
		raise (Error "not enough place in stack to push!")


(** Get a local variable value.
	@param state	State to update.
	@param i		Variable index.
	@return			Variable value.
	@raise Error	If the index is out of the stack. *)
let get (state: 'a state_t) i =
	try
		state.mem.(state.fp + i)
	with Invalid_argument _ ->
		raise (Error (sprintf "local variable index out of bounds: %d" i))


(** Get a global variable value.
	@param state	State to update.
	@param i		Global variable index.
	@return			Global variable value.
	@raise Error	If the index is out of the stack. *)
let get_glob (state: 'a state_t) i =
	try
		state.mem.(i)
	with Invalid_argument _ ->
		raise (Error (sprintf "global variable index out of bounds: %d" i))


(** Set a local variable value.
	@param state	State to update.
	@param i		Variable index.
	@param x		Value to set.
	@raise Error	If the index is out of the stack. *)
let set (state: 'a state_t) i x =
	try
		state.mem.(state.fp + i) <- x
	with Invalid_argument _ ->
		raise (Error (sprintf "local variable index out of bounds: %d" i))


(** Set a global variable value.
	@param state	State to update.
	@param i		Global variable index.
	@param x		Global value to set.
	@raise Error	If the index is out of the stack. *)
let set_glob (state: 'a state_t) i x =
	try
		state.mem.(i) <- x
	with Invalid_argument _ ->
		raise (Error (sprintf "global variable index out of bounds: %d" i))


(** Display the stack.
	@param state	State to display stack for. *)
let print_stack (state: 'a state_t) =
	let rec display p =
		if p < 0 then
			printf "[]"
		else begin
			printf "%d/%d" p state.mem.(p);
			printf (if p = state.fp then "|" else "::");
			display (p - 1)
		end in
	display (state.sp - 1)


(** Dump the provided state.
	@param state 	State to dump. *)
let print_state (state: 'a state_t) =
	printf "PC=%d, FP=%d, SP=%d, "	state.pc state.fp state.sp;
	print_stack state


(** Compute x^n. *)
let rec pow x n =
	if n = 0 then 1
	else if n mod 2 = 0 then
		let p = pow x (n / 2) in
		p * p
	else
		x * pow x (n - 1)


(** Execute the current instruction.
	@param state	Current state.
	@raise Error	If there is an error.*)
let step (state: 'a state_t) =
	let inst =
		try state.prog.code.(state.pc)
		with Invalid_argument _ ->
			raise (Error (sprintf "PC out of range: %04d" state.pc)) in
	state.pc <- state.pc + 1;
	match inst with
	| ADD ->
		let x = pop state in
		let y = pop state in
		push state (x + y)
	| SUB ->
		let x = pop state in
		let y = pop state in
		push state (x - y)
	| MUL ->
		let y = pop state in
		let x = pop state in
		push state (x * y)
	| DIV ->
		let x = pop state in
		let y = pop state in
		push state (x / y)
	| MOD ->
		let x = pop state in
		let y = pop state in
		push state (x mod y)
	| POW ->
		let x = pop state in
		let y = pop state in
		push state (pow x y)
	| PUSH k ->
		push state k
	| GET i ->
		push state (get state i)
	| SET i ->
		let x = pop state in
		set state i x
	| GOTO l ->
		state.pc <- l
	| GOTO_EQ l ->
		let x = pop state in
		let y = pop state in
		if x = y then state.pc <- l
	| GOTO_NE l ->
		let x = pop state in
		let y = pop state in
		if x <> y then state.pc <- l
	| GOTO_LT l ->
		let x = pop state in
		let y = pop state in
		if x < y then state.pc <- l
	| GOTO_LE l ->
		let x = pop state in
		let y = pop state in
		if x <= y then state.pc <- l
	| GOTO_GT l ->
		let x = pop state in
		let y = pop state in
		if x > y then state.pc <- l
	| GOTO_GE l ->
		let x = pop state in
		let y = pop state in
		if x >= y then state.pc <- l
	| INVOKE c ->
		state.env <- state.invoke state c state.env
	| CALL l ->
		push state state.fp;
		push state state.pc;
		state.pc <- l;
		state.fp <- state.sp
	| RESERVE n ->
		state.sp <- state.sp + n
	| RETURN_VOID n ->
		state.sp <- state.fp -n - 2;
		state.pc <- state.mem.(state.fp - 1);
		state.fp <- state.mem.(state.fp - 2)
	| RETURN n ->
		let res = state.mem.(state.sp - 1) in
		state.sp <- state.fp - n - 1;
		state.pc <- state.mem.(state.fp - 1);
		state.fp <- state.mem.(state.fp - 2);
		state.mem.(state.sp - 1) <- res
	| STOP ->
		state.pc <- state.pc - 1
	| DEBUG _
	| LABEL _ ->
		()
	| GET_GLOB i ->
		push state (get_glob state i)
	| SET_GLOB i ->
		let x = pop state in
		set_glob state i x




(** Get the current instruction.
	@param state	State to look in.
	@return			Current instruction.
	@raise Error	If the PC is out of bounds.*)
let current_inst state =
	try
		state.prog.code.(state.pc)
	with Invalid_argument _ ->
		raise (Error (sprintf "PC out of range: %d" state.pc))


(** Test if the program is ended.
	@param state	State to look in.
	@return			True if program is empty, false else. *)
let ended state =
		(current_inst state) = STOP


(** Get the environment from the state.
	@param state	State to look in.
	@return			Environment of the state. *)
let get_env state =
	state.env


(** Get the PC register.
	@param state	State.
	@return			PC value. *)
let get_pc state = state.pc


(** Get the FP register.
	@param state	State.
	@return			FP value. *)
let get_fp state = state.fp


(** Get the SP register.
	@param state	State.
	@return			SP value. *)
let get_sp state = state.sp


(** Run the VM in the state until the stop is found.
	@param state	State to run in. *)
let rec run state =
	step state;
	if not (ended state) then run state


(** Run the code and call f for each instruction to execute.
	@param state	VM state.
	@param f		Monitor function: state -> state *)
let rec run_monitor state f =
	let state = f state in
	if not (ended state) then begin
		step state;
		run_monitor state f
	end


(** Get the program.
	@param state	Current state.
	@return			State program. *)
let get_prog state =
	state.prog


(** Get the memory of the state.
	@param state	State to look in.
	@return			Memory. *)
let get_mem state =
	state.mem


