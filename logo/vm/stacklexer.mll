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

{

open Printf

open Common
open Stackparser

let ht: token StringHashtbl.t = StringHashtbl.create 17

let _ =
	List.iter
		(fun (k, v) -> Common.StringHashtbl.add ht k v)
		[
			("add",		ADD);
			("sub",		SUB);
			("mul",		MUL);
			("div",		DIV);
			("mod",		MOD);
			("pow",		POW);
			("push",	PUSH);
			("get",		GET);
			("set",		SET);
			("goto",	GOTO);
			("goto_eq",	GOTO_EQ);
			("goto_ne", GOTO_NE);
			("goto_lt", GOTO_LT);
			("goto_le", GOTO_LE);
			("goto_gt", GOTO_GT);
			("goto_ge", GOTO_GE);
			("call", 	CALL);
			("reserve",	RESERVE);
			("return",	RETURN);
			("return_void", RETURN_VOID);
			("invoke",	INVOKE);
			("stop",	STOP);
			("debug",	DEBUG);
			("false",	BOOL false);
			("true", 	BOOL true);
			("get_glob",GET_GLOB);
			("set_glob",SET_GLOB)
		]

let scan_id id =
	try
		StringHashtbl.find ht (String.lowercase_ascii id)
	with Not_found ->
		LABEL(id)
}

let comment = '@' [^ '\n' '\r']*
let space	= [' ' '\t' '\n' '\r']+
let id		= ['a' - 'z' 'A' - 'Z' '_']['a' - 'z' 'A' - 'Z' '_' '0' - '9']*
let nat		= ['0'-'9']+
let sign	= ['+' '-']?
let exp		= ['e' 'E'] sign nat
let int		= sign nat
let float	= sign (nat '.'| nat '.' nat | nat exp | nat '.' nat exp)

rule scan =
parse	int	as v				{ INT (int_of_string v) }
|		float as v				{ FLOAT (float_of_string v) }
|		id as s					{ scan_id s }
|		':'						{ COLON }
|		','						{ COMMA }
|		'#'						{ SHARP }
|		".meta"					{ META }
|		"\""					{ STR (str lexbuf) }
|		eof						{ EOF }
|		space					{ scan lexbuf }
|		comment					{ scan lexbuf }
|		_ as c					{ raise (LexerError (sprintf "unknown character '%c'" c)) }
and str =
parse	"\\\""					{ (String.make 1 '"') ^ (str lexbuf) }
|		("\\"[^ '"']) as s		{ s ^ (str lexbuf) }
|		'"'						{ "" }
|		[^'"' '\\']+ as s		{ s ^ (str lexbuf) }
|		eof						{ raise (Common.LexerError "EOF in middle of string") }
