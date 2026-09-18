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

module S = Tiny_httpd
module U = Tiny_httpd_util
open Printf
open Common
open Sys

exception BadURL

(* status codes *)
let sERROR = 0
let sOK = 1
let sBP = 2
let sSTOP = 3

(*	PROTOCOL

	HTTP:
		GOT 	/command?req
		ANSWER	JSON

	Commands:
	/continue		Execute instruction until stop or breakpoint.
		{ "status": STATUS, "pc": INT, "messages": [messages], "actions", actions }
	/bp?num=INT		Set a breakpoint.
		BOOL			-- True if the BP is set, false unset.
	/disassemble	Disassemble the program.
		[insttructions]
	/quit			Quit the server.
		none
	/regs			Return register values.
		[PC, SP, FP]
	/stack			Return current stack content.
		[stack items]
	/reset			Reset the state of the machine.
		{ "status": STATUS, "pc": INT, "messages": [messages], "actions", actions }
	/step			Execute one instruction.
		{ "status": STATUS, "pc": INT, "messages": [messages], "actions", actions }

Exec Message is:
		{
			"status": STATUS,				Status (see above).
			"pc": INT,						Current PC.
			"x": FLOAT, "y": FLOAT,			Turtle position.
			"head": FLOAT,					Turtle head.
			"sp": INT,						SP value
			"fp": INT,						FP value
			"messages": [messages],			Messages to display.
			"actions": [action],			Actions to perform.
			"stack": [INT]					Values of the stack
		}
 *)


(** Slice size. *)
let slice = ref 20

let asis s _ = output_string stderr s
(*let verbose f = f stderr; flush stderr*)
let verbose m = ()

(** Basic time-out (continue action). *)
let timeout = ref 0.2


(** Current env. *)
let current_env = ref (Logo.make ())

(** Current state. *)
let current_state = ref (Stackvm.new_state Stackinst.null Logo.syscall !current_env)

(** List of breakpoints *)
let break_points = ref []


(** Build a new VM.
	@param prog		Program to run inside.
	@return			Created VM. *)
let make prog =
	current_env := Logo.make ();
	Stackvm.new_state
		(Stackvm.get_prog !current_state)
		Logo.syscall
		!current_env


(** Check if ".." path is passed.
	@param s	Path to test.
	@return		True if a ".." path is used, false else. *)
let contains_dot_dot s =
	try
		String.iteri
			(fun i c ->
				if c='.' && i+1 < String.length s && String.get s (i+1) = '.' then raise Exit)
				s;
			false
	with Exit -> true


(** Run the browser on the served pages. *)
let display _ =
	verbose (asis "DEBUG: running command!\n");
	let url = "http://localhost:4040/index.html" in
	match Sys.os_type with
	| "Unix"
	| "Cygwin"->
		let ic = Unix.open_process_in "uname" in
		let uname = input_line ic in
		close_in ic;
		(match uname with
		| "Darwin" ->
			ignore (Sys.command ("open " ^ url))
		| _ ->
			ignore (Sys.command ("xdg-open " ^ url))
			(*ignore (Sys.command ("google-chrome " ^ url))*)
		)
	| "Win32" ->
		ignore (Sys.command ("start \"\" \"" ^ url ^ "\""))
	| _ ->
		printf "Run your browser with URL \"%s\"!" url;
	verbose (asis "Browser started!")


(** Bad syntax answer. *)
let bad_syntax _ =
	S.Response.fail ~code:404 "bad syntax"


(** Generate a JSON answer.
	@param t	JSOn text.
	@return		Answer. *)
let answer_json t =
	verbose (fun out -> fprintf out "ANSWER: %s\n" t);
	S.Response.make_string
		~headers:[("Content-Type", "application/json")]
		(Ok t)


(** Get the value of a register.
	@param state	State to look in.
	@param i		Register index.
	@return			Register value or -1. *)
let get_reg state i =
	try Stackvm.get state i
	with Stackvm.Error _ -> -1


(** Build an answer for an execurtion.
	@param status	Status of the execution.
	@param messages	Messages to display. *)
let answer_exec status messages =
	let sp = Stackvm.get_sp !current_state in

	let rec get_stack i =
		if i = sp then [] else
		(JINT (Stackvm.get_mem !current_state).(i))::(get_stack (i + 1)) in

	let actions = Logo.take_actions (Stackvm.get_env !current_state) in

	answer_json (json_to_string (
		JRECORD [
			("status", JINT status);
			("x", JFLOAT !current_env.Logo.x);
			("y", JFLOAT !current_env.Logo.y);
			("head", JFLOAT !current_env.Logo.head);
			("pc", JINT (Stackvm.get_pc !current_state));
			("sp", JINT (Stackvm.get_sp !current_state));
			("fp", JINT (Stackvm.get_fp !current_state));
			("messages", JARRAY (List.map (fun m -> JSTR m) messages));
			("actions", JARRAY actions);
			("stack", JARRAY (get_stack 0))
		]
	))


(** Perform the read action. *)
let do_quit server path req =
	verbose (asis "Quit!\n");
	exit 0


(** Output the list of quads in the program. *)
let do_disassemble server path req =
	let prog = Stackvm.get_prog !current_state in
	let insts = Stackinst.get_insts prog in
	let dis = map_n
		(fun i ->
			JSTR (sprintf "%04d: %s" i (Stackinst.to_string insts.(i))))
		 (Array.length insts) in
	answer_json (json_to_string (JARRAY dis))


(** Perform an execution step. *)
let do_step server path req =
	if Stackvm.ended !current_state then
		answer_exec sSTOP ["Program stopped!"]
	else begin
		Stackvm.step !current_state;
		answer_exec sOK []
	end


(** Reset the map. *)
let do_reset server path req =
	current_state := make (Stackvm.get_prog !current_state);
	answer_exec sOK []


(** Enable/disable a breakpoint. *)
let do_bp server args req =
	try
		let bp = int_of_string (List.assoc "num" (S.Request.query req)) in
		let ins = List.mem bp !break_points in
		if ins then
			break_points := List.filter (fun x -> x <> bp) !break_points
		else
			break_points := bp :: !break_points;
		answer_json (json_to_string (JBOOL (not ins)))
	with
	| Not_found
	| Failure _ ->
		bad_syntax ()


(** Start a continue action. *)
let do_continue server path req =
	let t0 = Sys.time () in

	let check_bp state =
		List.mem (Stackvm.get_pc state) !break_points in

	let rec run n =
		if Stackvm.ended !current_state then sSTOP else
		if (Sys.time ()) -. t0 >= !timeout then sERROR else
		if n == 0 then sOK
		else begin
			Stackvm.step !current_state;
			if check_bp !current_state then sBP else run (n - 1)
		end in

	answer_exec (run !slice) []


(** Generate stack content *)
let do_regs server path req =
	answer_json (json_to_string (JARRAY [
		JINT (Stackvm.get_pc !current_state);
		JINT (Stackvm.get_sp !current_state);
		JINT (Stackvm.get_fp !current_state)
	]))


(** Generate stack content. *)
let do_stack server path req =
	answer_json (json_to_string (JARRAY (
		map_array_to
			(fun x -> JINT x)
			(Stackvm.get_mem !current_state)
			(Stackvm.get_sp !current_state)
	)))


(** List of commands. *)
let commands = [
	("continue", do_continue);
	("bp", do_bp);
	("disassemble", do_disassemble);
	("quit", do_quit);
	("regs", do_regs);
	("stack", do_stack);
	("reset", do_reset);
	("step", do_step)
]


(** Get the MIME type for the path.
	@param path	Path to test.
	@return		MIME type. *)
let get_mime path =
	if String.ends_with path ~suffix:".html" then "text/html"
	else if String.ends_with path ~suffix:".svg" then "image/svg+xml"
	else "text/text"


(** Serve a file. *)
let serve_file server path req =

	let fail code msg =
		verbose (fun out -> fprintf out "DEBUG: %d: %s\n" code msg);
		S.Response.fail ~code:code "%s" msg in

	let path = if path = "" then "index.html" else path in
	let path = "pages/" ^ path in
	if contains_dot_dot path then
		fail 403 "Path is forbidden."
	else if not (Sys.file_exists path) then
		fail 404 "File not found."
	else if Sys.is_directory path then
		fail 404 "Cannot serve directory."
	else
		try
			let ic = open_in path in
			let mime = get_mime path in
			verbose (fun out -> fprintf out "DEBUG: MIME = %s\n" mime);
			let mime_type = ("Content-Type", get_mime path) in
			S.Response.make_raw_stream
				~headers:([mime_type])
				~code:200 (S.Byte_stream.of_chan ic)
		with e ->
			fail 500 (sprintf "error while reading file: %s" (Printexc.to_string e))


(** Serve the page at the given path.
	@param server	Current server.
	@param req		Current request.
	@param path		Current path.*)
let serve_page server path req =
	try
		verbose (fun out ->
			fprintf out "GOT: %s\n" path;
			List.iter (fun (k, v) -> fprintf out "DEBUG: %s=%s\n" k v) (S.Request.query req));
		(List.assoc path commands) server path req
	with
	| Not_found ->
		serve_file server path req
	| BadURL ->
		bad_syntax ()
	| Stackvm.Error msg ->
		answer_exec sERROR ["ERROR: " ^ msg]


(** Run the server. *)
let serve _ =
	let server = S.create
		~addr:		"127.0.0.1"
		~port: 		4040
		~startup:	display
		()
	in
	S.add_route_handler
		server ~meth:`GET
		S.Route.rest_of_path_urlencoded
		(serve_page server);
	match S.run server with
	| Ok () 	->
		()
	| Error e	->
		fatal_error (Printexc.to_string e)


(** Load the file.
	@param path		Path of the program.
	@return			Loaded program. *)
let load path =
	try
		let prog = Stackinst.load path in
		current_env := Logo.make ();
		Logo.init ();
		current_state := Stackvm.new_state prog Logo.syscall !current_env
	with
	| Stackinst.Error msg ->
		fatal_error msg


(** Program entry. *)

let _ =
	let free_args = ref [] in

	Arg.parse
		[]
		(fun f -> free_args := f :: !free_args)
		"logoapp <exec>: run application on the provided executable.";

	match !free_args with
	| [prog] ->
		load prog; serve ()
	| _ ->
		fatal_error "bad number of arguments!"

