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

open Common

let exec = ref ""

let opts = [
]
let doc = "Stack machine disassembly."

let free_arg s =
	if !exec <> ""
	then raise (Arg.Bad "Only one exec can be provided!")
	else exec := s

let _ =
	Arg.parse opts free_arg doc;
	if !exec = "" then
		fatal_error "ERROR: one exec must be provided!";
	Stackinst.print_prog (Stackinst.load !exec)
