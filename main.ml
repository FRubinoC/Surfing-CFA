(* This is the main code of the interpreter *)
open Printf
open Ast
open Sys
open Stdlib
open In_channel

let main =
	let lexbuf = Lexing.from_channel stdin in 
	let res =
	try
		Parser.main Lexer.token lexbuf
	with
		| Lexer.Error(c) ->
			fprintf stderr "Lexical char error at line %d: Unknown token '%c'\n" 
				lexbuf.lex_curr_p.pos_lnum c;
			exit 1
		| Lexer.ErrorStr(str) ->
			fprintf stderr "Lexical string error at line %d: Unknown token '%s'\n" 
				lexbuf.lex_curr_p.pos_lnum str;
			exit 1
		| Parser.Error ->
			fprintf stderr "Parsing error at line %d" lexbuf.lex_curr_p.pos_lnum;
			exit 1
	in
	let cfa_environment : Ast.cfa_env = [] in
	let abs_stores_env : Ast.abs_iot_stores = [] in
	let exe_env : Ast.exe_env = [] in
	let (set_cf, set_ase, set_ee) = Ast.frst_eval_IotStructure cfa_environment abs_stores_env exe_env res in
	let (simple_cfae, simple_ase, complex_cfae, complex_ase) = Ast.eval_IoTStructure set_cf set_ase set_ee in
	Ast.eval_attacks simple_cfae complex_cfae
	 
