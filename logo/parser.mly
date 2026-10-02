/*
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
 */

%{

	open Ast
	open Common
	open Printf

	module StringSet = Set.Make(String)

	(** Map of variable name over variable index. *)
	let var_map = ref StringMap.empty

	(** Get the index of variable.
		@param name	Name of variable.
		@return		Variable index, -1 if the variable does not exists. *)
	let get_var name =
		try
			StringMap.find name !var_map
		with Not_found ->
			-1

	(** Create a new variable.
		@param name		Name of variable.
		@return			Variable index. *)
	let make_var name =
		let index = Comp.alloc_var () in
		var_map := StringMap.add name index !var_map;
		index

	(** Local map. *)
	let local_set = ref StringSet.empty

	(** Record a variable as local.
		@param name		Variable name. *)
	let set_local name =
		local_set := StringSet.add name !local_set

	(** Test if a variable is local.
		@param name		Variable name.
		@return			True if the variable is local, false else. *)
	let is_local name =
		StringSet.mem name !local_set

	(** Map of the function. Provides the label of the function.*)
	let fun_map = ref StringMap.empty

	(** Get the label of a function.
		@param name		Function name. *)
	let get_fun name =
		try
			StringMap.find name !fun_map
		with Not_found ->
			-1

	(** Create a function.
		@param name		Name of function.
		@param ast		AST of the function.
		@return			Label of function. *)
	let make_fun name ast =
		let lab = Comp.new_label () in
		fun_map := StringMap.add name lab !fun_map;
		Comp.add_fun name lab ast

	(** Raise an error with the provided message.
		@param msg	Message to display. *)
	let error msg =
		raise (SyntaxError msg)

	(** Declare the passed parameters and assign them good offsets.
		@param params	Parameters to declare. *)
	let declare_params params =
		ignore (List.fold_left
			(fun index name ->
				var_map := StringMap.add name index !var_map;
				set_local name;
				index - 1
			)
			(-3)
			(List.rev params))

	(** Release the parameters.
		@param params	Parameters to declare. *)
	let release_params params =
		List.iter
			(fun name -> var_map := StringMap.remove name !var_map)
			params

%}

%token EOF

%token FORWARD
%token BACKWARD
%token PRINT
%token RIGHT
%token LEFT
%token HOME
%token MAKE
%token ADD
%token MINUS
%token MUL
%token DIV
%token MOD
%token POW

%token LEFT_PAR
%token RIGHT_PAR

%token<int> INT
%token<string> NAME
%token <string> REF
%token <string> PRINTS

%start program
%type<Ast.command> program

%%

program:
	opt_cmd_seq
		{ $1 }
;

opt_cmd_seq:
	/* empty */
		{ NOP }
|	cmd_seq
		{ $1 }
;

cmd_seq:
	cmd
		{ $1 }
| cmd_seq cmd
		{ SEQ($1, $2) }
;

cmd:
	FORWARD expr
		{ SYSCALL (Logo.cFORWARD, [$2]) }
|	RIGHT expr
		{ SYSCALL (Logo.cRIGHT, [$2]) }
| 	BACKWARD expr   
		{ SYSCALL (Logo.cBACKWARD, [$2]) } 
| 	LEFT expr
		{ SYSCALL (Logo.cLEFT, [$2]) }
|	PRINT expr
		{ PRINT $2 }
|	PRINTS
		{ PRINTS $1 }
| 	HOME
		{ SYSCALL (Logo.cHOME, []) }
|	MAKE NAME expr
		{ MAKE ((make_var $2), $3) }
;


expr:
	expr ADD mul_div_mod
		{ BINOP(OP_ADD,$1,$3) }
|	expr MINUS mul_div_mod
		{ BINOP(OP_SUB,$1,$3) }
|	mul_div_mod
		{ $1 }
;

val_ref_par:
	INT
		{ CST $1 }
|	REF
		{ let res = get_var $1 in if res = -1 then error "Unknown variable" else (VAR res) }
|	LEFT_PAR expr RIGHT_PAR
		{ $2 }
	
;

mul_div_mod :
	mul_div_mod MUL power
		{ BINOP(OP_MUL,$1,$3) }
|	mul_div_mod DIV power
		{ BINOP(OP_DIV,$1,$3) }
|	mul_div_mod MOD power
		{ BINOP(OP_MOD,$1,$3) }
|	power
		{ $1 }
;

power : 
	val_ref_par
		{ $1 }
|	val_ref_par POW power
		{ BINOP(OP_POW,$1,$3) }
;
