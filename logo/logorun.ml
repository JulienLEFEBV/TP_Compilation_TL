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

let _ =
	let dis = ref false in
	let verbose = ref false in
	let dump = ref false in
	let free = ref [] in

	let opts = [
		("-d", Arg.Set dis, "disassemble instructions");
		("-v", Arg.Set verbose, "display generated commands");
		("-D", Arg.Set dump, "dump performed actions at end")
	] in
	let doc = "Run the provided logo program." in

	let rec do_dump actions i last =
		if i >= last then () else begin
			printf "%s\n" (json_to_string actions.(i));
			do_dump actions (i + 1) last
		end in

	let rec run state last =
		if !dis then
			printf "%04d %s\n"
				(Stackvm.get_pc state)
				(Stackinst.to_string (Stackvm.current_inst state));
		Stackvm.step state;
		let sys = state.Stackvm.env in
		if !verbose && sys.Logo.cnt > last then
				do_dump sys.actions last sys.cnt;
		if Stackvm.ended state then ()
		else run state sys.cnt in

	let process path =
		let prog = Stackinst.load path in
		let sys = Logo.make () in
		Logo.init ();
		let state = Stackvm.new_state prog Logo.syscall sys in
		run state 0;
		if !dump then do_dump sys.actions 0 sys.cnt in

	Arg.parse opts (fun arg -> free := arg::!free) doc;
	List.iter process !free
