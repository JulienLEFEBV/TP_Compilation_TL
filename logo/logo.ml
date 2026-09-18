(*
 * logo -- logo compiler and VM.
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

let cFORWARD = 1		(* S::d ==> S -- move forward d units. *)
let cBACKWARD = 2		(* S::d ==> S -- move backward d units. *)
let cRIGHT = 3			(* S::a ==> S -- turn a° right. *)
let cLEFT = 4			(* S::a ==> S -- turn a° left. *)
let cPRINT = 5			(* S::x ==> S -- print x. *)
let cPRINTS = 6			(* S::i ==> -- print meta at index i. *)
let cHOME = 7			(* S ==> S -- move at initial position. *)
let cRANDOM = 8			(* S::k ==> S::random(k) *)

let init_x = 200.
let init_y = 200.
let init_head = 0.

exception BadCommand of string

type env = {
	mutable x: float;
	mutable y: float;
	mutable head: float;
	mutable cnt: int;
	actions: json array;
}

let deg2rad ang =
	ang *. Float.pi /. 180.

(** Get actions from the environment.
	@param env	Environment.
	@return		Take actions. *)
let take_actions env =
	let rec collect i =
		if i >= env.cnt then [] else
		env.actions.(i)::(collect (i + 1)) in
	let res = collect 0 in
	env.cnt <- 0;
	res

(** Build a new system state.
	@return	Initial system state. *)
let make _ =
	{
		x = init_x;
		y = init_y;
		head = init_head;
		cnt = 0;
		actions = Array.make 1000 JNONE;
	}

(** Add an action.
	@param sys		System state.
	@param action	Added action. *)
let add_action sys action =
	sys.actions.(sys.cnt) <- action;
	sys.cnt <- sys.cnt + 1


(** Undefined function. *)
let undef (state: env Stackvm.state_t) (sys: env) =
	raise (Invalid_argument "")


(** Forward action. *)
let forward state sys =
	let d = Stackvm.pop state in
	sys.x <- sys.x +. cos(deg2rad(sys.head +. 90.)) *. (float_of_int d);
	sys.y <- sys.y -. sin(deg2rad(sys.head +. 90.)) *. (float_of_int d);
	add_action sys
		(JRECORD [
			("action", JSTR "draw");
			("x", JFLOAT sys.x);
			("y", JFLOAT sys.y);
			("head", JFLOAT sys.head)
		])


(** Backward action. *)
let backward state sys =
	let d = Stackvm.pop state in
	sys.x <- sys.x -. cos(deg2rad(sys.head +. 90.)) *. (float_of_int d);
	sys.y <- sys.y +. sin(deg2rad(sys.head +. 90.)) *. (float_of_int d);
	add_action sys
		(JRECORD [
			("action", JSTR "draw");
			("x", JFLOAT sys.x);
			("y", JFLOAT sys.y);
			("head", JFLOAT sys.head)
		])


(** Turn right action. *)
let turn_right state sys =
	sys.head <- sys.head -. (float_of_int (Stackvm.pop state))


(** Turn left action. *)
let turn_left state sys =
	sys.head <- sys.head +. (float_of_int (Stackvm.pop state))


(** Print action. *)
let print state sys =
	add_action sys (JRECORD [
		("action", JSTR "print");
		("value", JINT (Stackvm.pop state))
	])

(** PrintS action. *)
let prints state sys =
	let meta = Stackvm.pop state in
	add_action sys (JRECORD [
		("action", JSTR "print");
		("value", JSTR (Stackinst.meta_to_string (Stackinst.get_meta (Stackvm.get_prog state) meta)))
	])

(** Home action. *)
let home state sys =
	sys.x <- init_x;
	sys.y <- init_y;
	sys.head <- init_head;
	add_action sys (JRECORD [
		("action", JSTR "move");
		("x", JFLOAT sys.x);
		("y", JFLOAT sys.y);
		("head", JFLOAT sys.head)
	])

(** random action. *)
let random state sys =
	Stackvm.push state (Random.int (Stackvm.pop state))

(** Table of system call functiions. *)
let funs = [|
		undef;	(* 0 *)
		forward;
		backward;
		turn_right;
		turn_left;
		print;
		prints;
		home;
		random
	|]


(** Implements system call for logo language.
	@param state	VM state.
	@param cmd		Command. *)
let syscall state cmd sys =
	try
		funs.(cmd) state sys;
		sys
	with Invalid_argument _ ->
		raise (BadCommand (sprintf "command %d undefined" cmd))


(** Initialize the Logo module .*)
let init _ =
	Random.self_init ()
