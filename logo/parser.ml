type token =
  | EOF
  | FORWARD
  | BACKWARD
  | PRINT
  | RIGHT
  | LEFT
  | HOME
  | MAKE
  | ADD
  | MINUS
  | MUL
  | DIV
  | MOD
  | POW
  | LEFT_PAR
  | RIGHT_PAR
  | INT of (
# 123 "parser.mly"
       int
# 22 "parser.ml"
)
  | NAME of (
# 124 "parser.mly"
       string
# 27 "parser.ml"
)
  | REF of (
# 125 "parser.mly"
        string
# 32 "parser.ml"
)
  | PRINTS of (
# 126 "parser.mly"
        string
# 37 "parser.ml"
)

open Parsing
let _ = parse_error;;
# 17 "parser.mly"

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

# 128 "parser.ml"
let yytransl_const = [|
    0 (* EOF *);
  257 (* FORWARD *);
  258 (* BACKWARD *);
  259 (* PRINT *);
  260 (* RIGHT *);
  261 (* LEFT *);
  262 (* HOME *);
  263 (* MAKE *);
  264 (* ADD *);
  265 (* MINUS *);
  266 (* MUL *);
  267 (* DIV *);
  268 (* MOD *);
  269 (* POW *);
  270 (* LEFT_PAR *);
  271 (* RIGHT_PAR *);
    0|]

let yytransl_block = [|
  272 (* INT *);
  273 (* NAME *);
  274 (* REF *);
  275 (* PRINTS *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\005\000\005\000\005\000\
\007\000\007\000\007\000\006\000\006\000\006\000\006\000\008\000\
\008\000\000\000"

let yylen = "\002\000\
\001\000\000\000\001\000\001\000\002\000\002\000\002\000\002\000\
\002\000\002\000\001\000\001\000\003\000\003\000\003\000\001\000\
\001\000\001\000\003\000\003\000\003\000\003\000\001\000\001\000\
\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\012\000\
\000\000\011\000\026\000\001\000\000\000\004\000\000\000\017\000\
\018\000\000\000\000\000\000\000\023\000\000\000\000\000\000\000\
\000\000\000\000\005\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\019\000\000\000\000\000\020\000\021\000\
\022\000\025\000"

let yydgoto = "\002\000\
\011\000\012\000\013\000\014\000\018\000\019\000\020\000\021\000"

let yysindex = "\255\255\
\024\255\000\000\001\255\001\255\001\255\001\255\001\255\000\000\
\245\254\000\000\000\000\000\000\024\255\000\000\001\255\000\000\
\000\000\025\255\011\255\003\255\000\000\025\255\025\255\025\255\
\025\255\001\255\000\000\255\254\001\255\001\255\001\255\001\255\
\001\255\001\255\025\255\000\000\011\255\011\255\000\000\000\000\
\000\000\000\000"

let yyrindex = "\000\000\
\018\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\032\000\000\000\000\000\000\000\
\000\000\077\000\020\000\001\000\000\000\084\000\103\000\110\000\
\129\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\136\000\000\000\039\000\058\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\024\000\254\255\006\000\000\000\234\255"

let yytablesize = 411
let yytable = "\001\000\
\024\000\022\000\023\000\024\000\025\000\026\000\029\000\030\000\
\039\000\040\000\041\000\042\000\028\000\036\000\015\000\034\000\
\016\000\002\000\017\000\016\000\031\000\032\000\033\000\035\000\
\003\000\004\000\005\000\006\000\007\000\008\000\009\000\003\000\
\029\000\030\000\037\000\038\000\027\000\000\000\014\000\000\000\
\000\000\000\000\010\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\015\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\006\000\000\000\000\000\000\000\
\000\000\000\000\000\000\008\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\010\000\000\000\
\000\000\000\000\000\000\000\000\000\000\007\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\009\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
\024\000\024\000\024\000\024\000\024\000\000\000\000\000\024\000\
\000\000\000\000\000\000\024\000\016\000\016\000\016\000\016\000\
\016\000\016\000\016\000\016\000\016\000\000\000\000\000\000\000\
\000\000\000\000\016\000\000\000\000\000\000\000\016\000\014\000\
\014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
\000\000\000\000\000\000\000\000\000\000\014\000\000\000\000\000\
\000\000\014\000\015\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\015\000\000\000\000\000\000\000\000\000\000\000\
\015\000\000\000\000\000\000\000\015\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\000\000\000\000\000\000\000\000\006\000\
\000\000\000\000\000\000\000\000\000\000\000\000\008\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\000\000\000\000\000\000\
\000\000\010\000\000\000\000\000\000\000\000\000\000\000\000\000\
\007\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\000\000\
\000\000\000\000\000\000\009\000\000\000\000\000\000\000\000\000\
\000\000\000\000\013\000"

let yycheck = "\001\000\
\000\000\004\000\005\000\006\000\007\000\017\001\008\001\009\001\
\031\000\032\000\033\000\034\000\015\000\015\001\014\001\013\001\
\016\001\000\000\018\001\000\000\010\001\011\001\012\001\026\000\
\001\001\002\001\003\001\004\001\005\001\006\001\007\001\000\000\
\008\001\009\001\029\000\030\000\013\000\255\255\000\000\255\255\
\255\255\255\255\019\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
\255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
\255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\000\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\255\255\255\255\015\001\
\255\255\255\255\255\255\019\001\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\008\001\009\001\255\255\255\255\255\255\
\255\255\255\255\015\001\255\255\255\255\255\255\019\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\255\255\255\255\255\255\255\255\255\255\015\001\255\255\255\255\
\255\255\019\001\001\001\002\001\003\001\004\001\005\001\006\001\
\007\001\008\001\009\001\255\255\255\255\255\255\255\255\255\255\
\015\001\255\255\255\255\255\255\019\001\001\001\002\001\003\001\
\004\001\005\001\006\001\007\001\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\255\255\255\255\255\255\255\255\019\001\
\255\255\255\255\255\255\255\255\255\255\255\255\019\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\001\001\002\001\
\003\001\004\001\005\001\006\001\007\001\255\255\255\255\255\255\
\255\255\019\001\255\255\255\255\255\255\255\255\255\255\255\255\
\019\001\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\001\001\002\001\003\001\004\001\005\001\006\001\007\001\255\255\
\255\255\255\255\255\255\019\001\255\255\255\255\255\255\255\255\
\255\255\255\255\019\001"

let yynames_const = "\
  EOF\000\
  FORWARD\000\
  BACKWARD\000\
  PRINT\000\
  RIGHT\000\
  LEFT\000\
  HOME\000\
  MAKE\000\
  ADD\000\
  MINUS\000\
  MUL\000\
  DIV\000\
  MOD\000\
  POW\000\
  LEFT_PAR\000\
  RIGHT_PAR\000\
  "

let yynames_block = "\
  INT\000\
  NAME\000\
  REF\000\
  PRINTS\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'opt_cmd_seq) in
    Obj.repr(
# 135 "parser.mly"
  ( _1 )
# 339 "parser.ml"
               : Ast.command))
; (fun __caml_parser_env ->
    Obj.repr(
# 140 "parser.mly"
  ( NOP )
# 345 "parser.ml"
               : 'opt_cmd_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cmd_seq) in
    Obj.repr(
# 142 "parser.mly"
  ( _1 )
# 352 "parser.ml"
               : 'opt_cmd_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cmd) in
    Obj.repr(
# 147 "parser.mly"
  ( _1 )
# 359 "parser.ml"
               : 'cmd_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'cmd_seq) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'cmd) in
    Obj.repr(
# 149 "parser.mly"
  ( SEQ(_1, _2) )
# 367 "parser.ml"
               : 'cmd_seq))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 154 "parser.mly"
  ( SYSCALL (Logo.cFORWARD, [_2]) )
# 374 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 156 "parser.mly"
  ( SYSCALL (Logo.cRIGHT, [_2]) )
# 381 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 158 "parser.mly"
  ( SYSCALL (Logo.cBACKWARD, [_2]) )
# 388 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 160 "parser.mly"
  ( SYSCALL (Logo.cLEFT, [_2]) )
# 395 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 162 "parser.mly"
  ( PRINT _2 )
# 402 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 164 "parser.mly"
  ( PRINTS _1 )
# 409 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    Obj.repr(
# 166 "parser.mly"
  ( SYSCALL (Logo.cHOME, []) )
# 415 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 168 "parser.mly"
  ( MAKE ((make_var _2), _3) )
# 423 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mul_div_mod) in
    Obj.repr(
# 174 "parser.mly"
  ( BINOP(OP_ADD,_1,_3) )
# 431 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mul_div_mod) in
    Obj.repr(
# 176 "parser.mly"
  ( BINOP(OP_SUB,_1,_3) )
# 439 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'mul_div_mod) in
    Obj.repr(
# 178 "parser.mly"
  ( _1 )
# 446 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 183 "parser.mly"
  ( CST _1 )
# 453 "parser.ml"
               : 'val_ref_par))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 185 "parser.mly"
  ( let res = get_var _1 in if res = -1 then error "Unknown variable" else (VAR res) )
# 460 "parser.ml"
               : 'val_ref_par))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 187 "parser.mly"
  ( _2 )
# 467 "parser.ml"
               : 'val_ref_par))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mul_div_mod) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'power) in
    Obj.repr(
# 193 "parser.mly"
  ( BINOP(OP_MUL,_1,_3) )
# 475 "parser.ml"
               : 'mul_div_mod))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mul_div_mod) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'power) in
    Obj.repr(
# 195 "parser.mly"
  ( BINOP(OP_DIV,_1,_3) )
# 483 "parser.ml"
               : 'mul_div_mod))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mul_div_mod) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'power) in
    Obj.repr(
# 197 "parser.mly"
  ( BINOP(OP_MOD,_1,_3) )
# 491 "parser.ml"
               : 'mul_div_mod))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'power) in
    Obj.repr(
# 199 "parser.mly"
  ( _1 )
# 498 "parser.ml"
               : 'mul_div_mod))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'val_ref_par) in
    Obj.repr(
# 204 "parser.mly"
  ( _1 )
# 505 "parser.ml"
               : 'power))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'val_ref_par) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'power) in
    Obj.repr(
# 206 "parser.mly"
  ( BINOP(OP_POW,_1,_3) )
# 513 "parser.ml"
               : 'power))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.command)
