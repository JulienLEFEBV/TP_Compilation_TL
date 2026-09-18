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

{

	open Common
	open Parser
	open Printf

	let line = ref 1
}


let blank = [' ' '\t' '\r']
let digit = ['0'-'9']
let dec = digit+
let letter = ['a'-'z' 'A'-'Z']
let id = (letter | '_')(letter | digit | '_')*

rule token = parse
	'\n'		{ incr line; token lexbuf }
|	blank		{ token lexbuf }


|	"forward"	{ FORWARD }
|	"print"		{ PRINT }
|	"right"		{ RIGHT }


|	"print" blank* '"' ([^ '"']* as x) '"'
				{ PRINTS x }

|	dec	as n	{ INT (int_of_string n) }

|	';' [^'\n']* '\n'	{ incr line; token lexbuf }
|	eof					{ EOF }
|	_ as c				{ raise (LexerError (sprintf "illegal char '%c'" c)) }
