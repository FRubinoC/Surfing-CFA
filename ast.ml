(* Abstract Syntax Tree *)
open Printf
open List
open Random
open Unix
open Stdlib

type encryption = 
	| Clear
	| Encrypted

type value =
	| None
	| Eint of int
	| Ebool of bool
	| Estring of string
	| Echar of char
	

type term =
	| ParallelTerms of term * term
	| Value of value
	| SensorLoc of string
	| Variable of string
	| Funct of string * term
	| Add of term * term
	| Sub of term * term
	| Mul of term * term
	| Div of term * term
	
	
type actuator = 
	| InactiveActuator
	| ActuatorIntAction of actuator
	| ActuatorCommand of term * actuator
	| ACloseIter
	| AOpenIter of actuator


type sensor =
	| InactiveSensor
	| SensorIntAction of sensor
	| SensorStore of string * int * int * sensor
	| SCloseIter
	| SOpenIter of sensor
	

type node_number =
	| NoNode
	| NodeNumber of int
	| ParallelNodeNumber of node_number * node_number


type condition = 
	| Bool of bool
	| Greater of term * term
	| Lower of term * term
	| EqGr of term * term
	| EqLw of term * term
	| Equal of term * term
	| And of condition * condition
	| Or of condition * condition


type process =
	| InactProcess
	| MultiOutput of  string * term * node_number * process
	| InputProc of int * string * term * process
	| ConditionProc of condition * process * process * process
	| Assignment of string * term * process
	| ActivateActuator of string * string * process
	| PCloseIter
	| POpenIter of process
	| Wait of string * process
	| Release of string * process
	| Used of string * process
	| Prepare of string * process
	| Encrypt of string * process
	| Decrypt of string * process


type component =
	| InactiveComponent
	| Process of string * process
	| Sensor of string * sensor
	| Actuator of string * actuator
	| ParallelComponent of component * component
	

type node =
	| InactNode
	| Node of int * component
	| ParallelNodes of node * node


type funct_definition =
	| InactFunction
	| ParallelFunctions of funct_definition * funct_definition
	| FunctionDefinition of string * term * term


type declaration =
	| ParallelDeclarations of declaration * declaration
	| SensorDeclaration of string
	| ActuatorDeclaration of string
	| VariableDeclaration of string
	

type node_declaration =
	|	ParallelNodesDeclaration of node_declaration * node_declaration
	| Store of int * declaration


type iot_structure =
	| IoTStructure of node_declaration * funct_definition * node


(* Exceptions *)









(* Environmental structures *)
type abs_term =
	| AbsUndefined of string
	| AbsSen of string * int
	| AbsConst of value
	| AbsFun of string * ((abs_term list) list)
	| AbsAdd of (abs_term list) * (abs_term list)
	| AbsSub of (abs_term list) * (abs_term list)
	| AbsMul of (abs_term list) * (abs_term list)
	| AbsDiv of (abs_term list) * (abs_term list)
	| AbsCut of string
	| AbsInput of int * string * int
	| AbsOrigin of int * string * (abs_term list)
	| AbsConstDep of value * (abs_term list)

type abs_condition =
	| CNone
	| CBool of bool
	| CAnd of abs_condition * abs_condition
	| COr of abs_condition * abs_condition
	| CNot of abs_condition
	| CEqual of (abs_term list) * (abs_term list)
	| CGreater of (abs_term list) * (abs_term list)
	| CLower of (abs_term list) * (abs_term list)
	| CEqGr of (abs_term list) * (abs_term list)
	| CEqLw of (abs_term list) * (abs_term list)


(*CFA data structures*)
type node_cfa_env =
{
	abs_cfa_store : (string * (abs_term list)) list;
	abs_net_env : (int * string * ((abs_term list) list)) list;
	abs_data_coll : abs_term list;
	abs_actuator_dec : (string * ((string * (abs_condition list)) list)) list;
}
type cfa_env = (int * node_cfa_env) list


(* Abstract execution store *)
type abs_store =
{
	abs_var : (string * (abs_term list)) list;
	abs_input : (int * string * ((abs_term list) list)) list;
	abs_release : string list;
}

type abs_iot_stores = (int * abs_store) list


(* Process execution storage *)
type stop =
	| Free
	| StopWait of string

type proc_env = string * process * stop
type exe_env = (int * (proc_env list)) list




(************************ PRINT FUNCTIONS *******************************)
let pprint_value = function
	| None -> "N"
	| Eint(i) -> sprintf "I(%i)" i
	| Ebool(b) -> sprintf "B(%b)" b
	| Estring(str) -> sprintf "S(%s)" str
	| Echar(ch) -> sprintf "C(%c)" ch	

let rec pprint_str_list (str_list : string list) : string =
	match str_list with
	| str::res_list ->
		str ^ "| " ^ (pprint_str_list res_list)
	| [] -> ""

let rec pprint_absterms (terms : abs_term list) : string =
	match terms with
	| frst_term::res_terms ->
		begin
			match frst_term with
			| AbsUndefined(a) -> "Undefined(" ^ a ^ ");" ^ (pprint_absterms res_terms)
			| AbsSen(sen,node) -> "SensorValue(" ^ sen ^ "," ^ (string_of_int node) ^ ");" ^ (pprint_absterms res_terms)
			| AbsConst(value) -> "Constant(" ^ (pprint_value value) ^ ");" ^ (pprint_absterms res_terms)
			| AbsConstDep(value, abs_term1) -> "ConstantDependant(" ^ (pprint_value value) ^ ",[" ^ (pprint_absterms abs_term1) ^ "]);" ^ (pprint_absterms res_terms)
			| AbsAdd(a,b) -> "([" ^ (pprint_absterms a) ^ "] PLUS [" ^ (pprint_absterms b) ^ "]);" ^ (pprint_absterms res_terms)
			| AbsSub(a,b) -> "([" ^ (pprint_absterms a) ^ "] MINUS [" ^ (pprint_absterms b) ^ "]);" ^ (pprint_absterms res_terms)
			| AbsMul(a,b) -> "([" ^ (pprint_absterms a) ^ "] MULTIPLIED BY [" ^ (pprint_absterms b) ^ "]);" ^ (pprint_absterms res_terms)
			| AbsDiv(a,b) -> "([" ^ (pprint_absterms a) ^ "] DIVIDED BY [" ^ (pprint_absterms b) ^ "]);" ^ (pprint_absterms res_terms)
			| AbsInput(src, const, count) -> "Input(" ^ (string_of_int src) ^ "," ^ const ^ "," ^ (string_of_int count) ^ ");" ^ (pprint_absterms res_terms)
			| AbsCut(a) -> "AbsCut(" ^ a ^ ")" ^ ";" ^ (pprint_absterms res_terms)
			| AbsOrigin(src, const, terms) -> "Origin(" ^ (string_of_int src) ^ "," ^ const ^ ",[" ^ (pprint_absterms terms) ^ "]);" ^ (pprint_absterms res_terms)
			| AbsFun(name, params) ->
					let str_list = List.map (pprint_absterms) params in
					let compact = pprint_str_list str_list in
					"Function(" ^ compact ^ ") " ^ (pprint_absterms res_terms)
		end
	| [] -> ""

let rec pprint_condition (cond : abs_condition) : string =
	match cond with
	| CNone -> "CNone"
	| CNot(a) -> "NOT(" ^ (pprint_condition a) ^ ")"
	| CBool(a) -> "BOOL(" ^ (string_of_bool a) ^ ")"
	| CAnd(a,b) -> "(" ^ (pprint_condition a) ^ ") AND (" ^ (pprint_condition b) ^ ")"
	| COr(a,b) -> "(" ^ (pprint_condition a) ^ ") OR (" ^ (pprint_condition b) ^ ")"
	| CEqual(a,b) -> "([" ^ (pprint_absterms a) ^ "]) EQUAL TO ([" ^ (pprint_absterms b) ^ "])"
	| CGreater(a,b) -> "([" ^ (pprint_absterms a) ^ "]) GREATER THAN ([" ^ (pprint_absterms b) ^ "])"
	| CLower(a,b) -> "([" ^ (pprint_absterms a) ^ "]) LOWER THAN ([" ^ (pprint_absterms b) ^ "])"
	| CEqGr(a,b) -> "([" ^ (pprint_absterms a) ^ "]) GREATER THAN OR EQUAL TO ([" ^ (pprint_absterms b) ^ "])"
	| CEqLw(a,b) -> "([" ^ (pprint_absterms a) ^ "]) LOWER THAN OR EQUAL TO ([" ^ (pprint_absterms b) ^ "])"


let rec pprint_alternative_condition (conds : abs_condition list) (counter : int)	: string =
	match conds with
	| frst_cond::res_conds ->
		"Alternative " ^ (string_of_int counter) ^ ": " ^ (pprint_condition frst_cond) ^ "\n" ^ (pprint_alternative_condition res_conds (counter+1))
	| [] -> ""



let rec pprint_commands (cmds : ((string * (abs_condition list)) list)) : string =
	match cmds with
	| (frst_cmd, cond)::res_cmds ->
			 ("\nCommand " ^ frst_cmd ^ ": \n" ^ (pprint_alternative_condition cond 1) ^ "\n") ^ (pprint_commands res_cmds)
	| [] -> ""


let rec pprint_decisions (actuators_dec : (string * ((string * (abs_condition list)) list)) list) : string =
	match actuators_dec with
	| (act, cmds)::res_act ->
		("\n\n---------------------\n Actuator " ^ act ^ ":\n") ^ (pprint_commands cmds) ^ (pprint_decisions res_act)
	| [] -> ""


let rec pprint_actuator_stats (cfae : cfa_env) : unit =
	match cfae with
	| node_cfa::res_cfae ->
		begin
			match node_cfa with
			| (nnode, cfa_env) ->
				let path = "./results" in
				let oc = open_out_gen [Open_append; Open_creat] 0o666 path in
				let _ = Printf.fprintf oc "%s" ("\n*************************\n\nActuation Decision Node " ^ (string_of_int nnode) ^ ":\n") in
				let _ = Printf.fprintf oc "%s" (pprint_decisions cfa_env.abs_actuator_dec) in
				pprint_actuator_stats res_cfae
		end
	| [] -> ()


let rec pprint_ext_actuator_stats (cfae : cfa_env) : unit =
	match cfae with
	| node_cfa::res_cfae ->
		begin
			match node_cfa with
			| (nnode, cfa_env) ->
				let path = "./extresults" in
				let oc = open_out_gen [Open_append; Open_creat] 0o666 path in
				let _ = Printf.fprintf oc "%s" ("\n*************************\n\nActuation Decision Node " ^ (string_of_int nnode) ^ ":\n") in
				let _ = Printf.fprintf oc "%s" (pprint_decisions cfa_env.abs_actuator_dec) in
				pprint_ext_actuator_stats res_cfae
		end
	| [] -> ()


let rec pprint_final_actuator_stats (cfae : cfa_env) : unit =
	match cfae with
	| node_cfa::res_cfae ->
		begin
			match node_cfa with
			| (nnode, cfa_env) ->
				let path = "./finalresults" in
				let oc = open_out_gen [Open_append; Open_creat] 0o666 path in
				let _ = Printf.fprintf oc "%s" ("\n*************************\n\nActuation Decision Node " ^ (string_of_int nnode) ^ ":\n") in
				let _ = Printf.fprintf oc "%s" (pprint_decisions cfa_env.abs_actuator_dec) in
				pprint_final_actuator_stats res_cfae
		end
	| [] -> ()


let rec pprint_node_store (nnode : int) (store : (string * (abs_term list)) list) : unit =
	match store with
	| (var, abs_term)::res_vars ->
		let _ = Printf.printf "%s" (var ^ ": " ^ (pprint_absterms abs_term)^ ";\n") in
		pprint_node_store nnode res_vars
	| [] -> ()


let rec pprint_absterm_list (terms_list : (abs_term list) list) : string =
	match terms_list with
	| frst_term::res_terms ->
		if (res_terms = []) then pprint_absterms frst_term
		else (pprint_absterms frst_term) ^ "][" ^ (pprint_absterm_list res_terms)
	| [] -> ""


let rec pprint_stat_inputs (inputs : (int * string * ((abs_term list) list)) list) : string =
	match inputs with
	| frst_input::res_inputs ->
			begin
				match frst_input with
				| (src, const, terms) ->
						"Input(" ^ (string_of_int src) ^ "," ^ const ^ ",{[" ^ (pprint_absterm_list terms) ^ "]})\n" ^ (pprint_stat_inputs res_inputs)
			end
	| [] -> "\n"


let rec pprint_input_stats (cfae : cfa_env) : unit =
	match cfae with
	| node_cfa::res_cfae ->
		begin
			match node_cfa with
			| (nnode, cfa_env) ->
				let path = "./inputs" in
				let oc = open_out_gen [Open_append; Open_creat] 0o666 path in
				let _ = Printf.fprintf oc "%s" ("\n*************************\n\n Input Node " ^ (string_of_int nnode) ^ ":\n") in
				let _ = Printf.fprintf oc "%s" (pprint_stat_inputs cfa_env.abs_net_env) in
				pprint_input_stats res_cfae
		end
	| [] -> ()


(* ********************** UTILITY FUNCTIONS *******************)


let rec concatenate_string_list (str_list : string list) : string =
	match str_list with
	| str::res_str -> str ^ "," ^ (concatenate_string_list res_str)
	| [] -> ""
	

let rec string_of_absterms (abs_terms : abs_term list) : string =
	match abs_terms with
	| frst_term::res_terms ->
		begin
			match frst_term with
			| AbsUndefined(a) -> ("Undefined") ^ a ^ "," ^ (string_of_absterms res_terms)
			| AbsSen(sen, num_node) -> "(Sen" ^ sen ^ (string_of_int num_node) ^ "," ^ (string_of_absterms res_terms) ^ ")"
			| AbsConst(value) -> "(Const" ^ (pprint_value value) ^ "," ^ (string_of_absterms res_terms) ^ ")"
			| AbsFun(name, params_terms) -> 
				let list_string_param = List.map (string_of_absterms) params_terms in
				let conc_strings = concatenate_string_list list_string_param in
				"(Fun" ^ name ^ conc_strings ^ "," ^ (string_of_absterms res_terms) ^ ")"
			| AbsConstDep(value, abs_term1) -> "(ConstDep(" ^ (pprint_value value) ^ ",[" ^ (string_of_absterms abs_term1) ^ "])," ^ (string_of_absterms res_terms) ^ ")"
			| AbsAdd(abs_term1, abs_term2) -> "(Add(" ^ (string_of_absterms abs_term1) ^ "," ^ (string_of_absterms abs_term2) ^ ")," ^ (string_of_absterms res_terms) ^ ")"
			| AbsSub(abs_term1, abs_term2) -> "(Sub(" ^ (string_of_absterms abs_term1) ^ "," ^ (string_of_absterms abs_term2) ^ ")," ^ (string_of_absterms res_terms) ^ ")"
			| AbsMul(abs_term1, abs_term2) -> "(Mul(" ^ (string_of_absterms abs_term1) ^ "," ^ (string_of_absterms abs_term2) ^ ")," ^ (string_of_absterms res_terms) ^ ")"
			| AbsDiv(abs_term1, abs_term2) -> "(Div(" ^ (string_of_absterms abs_term1) ^ "," ^ (string_of_absterms abs_term2) ^ ")," ^ (string_of_absterms res_terms) ^ ")"
			| AbsCut(str) -> "Cut" ^ str ^ "," ^ (string_of_absterms res_terms)
			| AbsOrigin(src, const, terms) -> "Origin(" ^ (string_of_int src) ^ "," ^ const ^ ",[" ^ (string_of_absterms terms) ^ "])," ^ (string_of_absterms res_terms)
			| AbsInput(src, const, counter) -> "(Input(" ^ (string_of_int src) ^ "," ^ const ^ "," ^ (string_of_int counter) ^ ")," ^ (string_of_absterms res_terms) ^ ")"
		end
	| [] -> ""


let rec string_of_absterm (abs_term : abs_term) : string =
	match abs_term with
	| AbsUndefined(a) -> ("(Undefined") ^ a ^ ")"
	| AbsSen(sen, num_node) -> "(Sen" ^ sen ^ (string_of_int num_node) ^ ")"
	| AbsConst(value) -> "(Const" ^ (pprint_value value) ^ ")"
	| AbsFun(name, params_terms) -> 
		let list_string_param = List.map (string_of_absterms) params_terms in
		let conc_strings = concatenate_string_list list_string_param in
		"(Fun" ^ name ^ conc_strings ^ ")"
	| AbsConstDep(value, abs_term1) -> "(ConstDep(" ^ (pprint_value value) ^ "," ^ (string_of_absterms abs_term1) ^ "]))"
	| AbsAdd(abs_term1, abs_term2) -> "(Add(" ^ (string_of_absterms abs_term1) ^ "," ^ (string_of_absterms abs_term2) ^ "))"
	| AbsSub(abs_term1, abs_term2) -> "(Sub(" ^ (string_of_absterms abs_term1) ^ "," ^ (string_of_absterms abs_term2) ^ "))"
	| AbsMul(abs_term1, abs_term2) -> "(Mul(" ^ (string_of_absterms abs_term1) ^ "," ^ (string_of_absterms abs_term2) ^ "))"
	| AbsDiv(abs_term1, abs_term2) -> "(Div(" ^ (string_of_absterms abs_term1) ^ "," ^ (string_of_absterms abs_term2) ^ "))"
	| AbsCut(str) -> "(Cut" ^ str ^ ")"
	| AbsOrigin(src, const, terms) -> "(Origin(" ^ (string_of_int src) ^ "," ^ const ^ ",[" ^ (string_of_absterms terms) ^ "]))"
	| AbsInput(src, const, counter) -> "(Input(" ^ (string_of_int src) ^ "," ^ const ^ "," ^ (string_of_int counter) ^ "))"


let rec string_list_of_absterms (abs_terms : abs_term list)	: string list =
	match abs_terms with
	| frst_term::res_terms ->
			(string_of_absterm frst_term)::(string_list_of_absterms res_terms)
	| [] -> []


let rec check_str_in_list (str : string) (string_list : string list) : bool =
	match string_list with
	| frst_str::res_strings ->
			if (str = frst_str) then true
			else check_str_in_list str res_strings
	| [] -> false


let rec merge_abs_term_lists (strlist1 : string list) (terms1 : abs_term list) (strlist2 : string list) (base_terms : abs_term list) : abs_term list =
	match strlist1 with
	| str::res_str ->
		begin
			match terms1 with
			| frst_term::res_terms ->
				if (check_str_in_list str strlist2) then 
					merge_abs_term_lists res_str res_terms strlist2 base_terms
				else 
					merge_abs_term_lists res_str res_terms strlist2 (frst_term::base_terms)
			| [] -> failwith("Error during merge abs_term_lists: the string list doesn't have the same number of elements of the abstract terms list")
		end
	| [] -> 
		base_terms


let rec check_abs_iot_store (nnode : int) (abs_store_env : abs_iot_stores) : bool =
	match abs_store_env with
	| (num_store, _)::res_env ->
		if (num_store = nnode) then true
		else check_abs_iot_store nnode res_env
	| [] -> false		


let rec	add_node_abs_iot_store (nnode : int) (abs_store_env : abs_iot_stores) : abs_iot_stores =
	if (check_abs_iot_store nnode abs_store_env) then failwith("Error: node already present during abs_iot_store node insertion")
	else begin
		let new_abs_store =
			{
				abs_var = [];
				abs_input = [];
				abs_release = [];
			} in
		(nnode, new_abs_store)::abs_store_env
	end


let rec check_cfa_node_presence (nnode : int) (cfa_environment : cfa_env) : bool =
	match cfa_environment with
	| (ncfa_node, env)::res_cfa_env ->
		if ncfa_node = nnode then true
		else check_cfa_node_presence nnode res_cfa_env
	| [] -> false


let rec add_node_cfa_env (nnode : int) (cfa_environment : cfa_env) : cfa_env =
	if (check_cfa_node_presence nnode cfa_environment) then failwith("Error: node already present during cfa node insertion")
	else begin
		let new_cfa_node =
			{
				abs_cfa_store = [];
				abs_net_env = [];
				abs_data_coll = [];
				abs_actuator_dec = [];
			} in
		(nnode, new_cfa_node)::cfa_environment
	end


let rec check_exe_env_presence (nnode : int) (exe_env : exe_env) : bool =
	match exe_env with
	| (exe_node, _)::res_env -> 
		if exe_node = nnode then true
		else check_exe_env_presence nnode res_env
	| [] -> false


let rec add_node_exe_env (nnode : int) (exe_env : exe_env) : exe_env =
	if (check_exe_env_presence nnode exe_env) then failwith("Error: node already present during execution environment node insertion")
	else (nnode, [])::exe_env


let rec update_ase (nnode : int) (new_node_ase : abs_store) (ase : abs_iot_stores) : abs_iot_stores =
	match ase with
	| (num, store)::res_ase ->
			if (num = nnode) then (nnode, new_node_ase)::res_ase
			else (num, store)::(update_ase nnode new_node_ase res_ase)
	| [] -> failwith("Error during update_ase: node_ase not existent")


let rec take_node_ase (nnode : int) (ase : abs_iot_stores) : abs_store =
	match ase with
	| (num, store)::res_stores ->
		if (num = nnode) then store
		else take_node_ase nnode res_stores
	| [] -> failwith("Error: not existent node ase during take_node_ase operation")


let rec take_node_cfa (nnode : int) (cfa_env : cfa_env) : node_cfa_env =
	match cfa_env with
	| (numNode, node_cfa)::res_cfa_env ->
		if (numNode = nnode) then node_cfa
		else take_node_cfa nnode res_cfa_env
	| [] -> failwith("Error: node inexistent during take_node_cfa operation")


let rec check_cfa_act_presence (name : string) (act_dec_list : (string * ((string * (abs_condition list)) list)) list) : bool =
	match act_dec_list with
	| (actloc, _)::res_list ->
			if (actloc = name) then true
			else check_cfa_act_presence name res_list
	| [] -> false


let rec update_node_cfa (nnode : int) (new_cfa : node_cfa_env) (cfa_env : cfa_env) : cfa_env =
	match cfa_env with
	| (numNode, ncfa)::res_node_cfa ->
		if numNode = nnode then (nnode, new_cfa)::res_node_cfa
		else (numNode, ncfa)::(update_node_cfa nnode new_cfa res_node_cfa)
	| [] -> failwith("Error: update of inexistent node")

	
let rec add_actuator_cfa (name : string) (nnode : int) (cfa_env : cfa_env) : (cfa_env) =
	let node_cfa = take_node_cfa nnode cfa_env in
	if (check_cfa_act_presence name node_cfa.abs_actuator_dec) then failwith("Error: actuator already registered in the cfa data structure")
	else
	let new_node_cfa =
		{
			abs_cfa_store = node_cfa.abs_cfa_store;
			abs_net_env = node_cfa.abs_net_env;
			abs_data_coll = node_cfa.abs_data_coll;
			abs_actuator_dec = (name, [])::(node_cfa.abs_actuator_dec)
		} in
		update_node_cfa nnode new_node_cfa cfa_env


let rec check_var_existance_cfa_node (var : string) (abs_cfa_store : (string * (abs_term list)) list) : bool =
	match abs_cfa_store with
	| (v, _)::res_store ->
			if v = var then true
			else check_var_existance_cfa_node var res_store
	| [] -> false
	

let rec add_var_ase_store (var : string) (nnode : int) (ase : abs_iot_stores) : abs_iot_stores =
	let node_ase = take_node_ase nnode ase in
	let new_node_ase =
		{
			abs_var = (var, AbsUndefined(var)::[])::node_ase.abs_var;
			abs_input = node_ase.abs_input;
			abs_release = node_ase.abs_release;
		} in
	update_ase nnode new_node_ase ase


let rec add_var_cfa_env (var : string) (nnode : int) (cfae : cfa_env) : cfa_env	=
	let node_cfa = take_node_cfa nnode cfae in
	if check_var_existance_cfa_node var node_cfa.abs_cfa_store then failwith("Error: variable already inserted in node cfa structure")  
	else
		let new_node_cfa =
			{
				abs_cfa_store = (var, AbsUndefined(var)::[])::node_cfa.abs_cfa_store;
				abs_net_env = node_cfa.abs_net_env;
				abs_data_coll = node_cfa.abs_data_coll;
				abs_actuator_dec = node_cfa.abs_actuator_dec;
			} in
		update_node_cfa nnode new_node_cfa cfae


let rec add_process_exe_env (nnode : int) (new_proc : proc_env) (exe_env : exe_env) : exe_env =
	match exe_env with
	| (node, list_process)::res_nodes ->
			if node = nnode then 
				begin	(node, new_proc::list_process)::res_nodes
				end
			else (node, list_process)::(add_process_exe_env nnode new_proc res_nodes)			
	| [] -> failwith("Error during process insertion in exe_env, node not present")	
		

let rec add_commands_to_act (commands : string list) : (string * (abs_condition list)) list =
	match commands with
	| cmd::res_cmd -> (cmd, [])::(add_commands_to_act res_cmd)
	| [] -> []


let rec take_abs_releases (nnode : int) (ase : abs_iot_stores) : string list =
	match ase with
	| (numnode, store)::res_stores ->
			if (numnode = nnode) then store.abs_release
			else take_abs_releases nnode res_stores
	| [] -> failwith("Error: not existient node structure for take_abs_releases")


let rec update_node_ee (nnode : int) (proc_list : proc_env list) (ee : exe_env) : (exe_env) =
	match ee with
	| (numNode, old_proc_env)::res_ee ->
			if (nnode = numNode) then (numNode, proc_list)::res_ee
			else (numNode, old_proc_env)::(update_node_ee nnode proc_list res_ee)
	| [] -> failwith("Error: cannot update node execution environemnt, node structure not present")


let rec take_node_ee (nnode : int) (ee : exe_env) : proc_env list =
	match ee with
	| (numnode, proc_env)::res_ee ->
			if (numnode = nnode) then proc_env
			else take_node_ee nnode res_ee
	| [] -> failwith("Error during take_node_ee: node not found")	


let rec take_new_node_ee (nnode : int) (ee : exe_env) : int * (proc_env list) =
	match ee with
	| (num_node, proc_env)::res_ee ->
			if (num_node = nnode) then take_new_node_ee nnode res_ee
			else (num_node, proc_env)
	| [] -> failwith("Error take_new_node_ee: no nodes available")


let rec check_release_happened (var : string) (releases : string list) : bool =
	match releases with
	| rel::res_rel ->
			if (rel = var) then true
			else check_release_happened var res_rel
	| [] -> false


let rec extract_node_process (nnode : int) (node_ee : proc_env list) (abs_releases : string list) (old_proc_list : proc_env list) : (proc_env * (proc_env list)) =
	match node_ee with
	| frst_proc::res_proc -> 
		begin
			match frst_proc with
			| (name, ast, stop) ->
					begin
						match stop with
						| Free -> (frst_proc, old_proc_list@res_proc)
						| StopWait(var) -> 
							if (check_release_happened var abs_releases) then ((name, ast, Free), old_proc_list@res_proc)
							else extract_node_process nnode res_proc abs_releases (frst_proc::old_proc_list)
					end
		end
	| [] -> failwith("Error during extract_node_process: the node have blocked processes but no available processes to evaluate")
			


let rec extract_process (nnode : int) (ee : exe_env) (abs_releases : string list) : (proc_env * exe_env) =
	let node_ee = take_node_ee nnode ee in
	let (exe_proc, exe_node) = extract_node_process nnode node_ee abs_releases [] in
	(exe_proc, (update_node_ee nnode exe_node ee))

let rec remove_node_ee (nnode : int) (ee : exe_env) : exe_env =
	match ee with
	| (numnode, proc_env)::res_node_ee ->
			if (numnode = nnode) then res_node_ee
			else (numnode, proc_env)::(remove_node_ee nnode res_node_ee)
	| [] -> failwith("Error during remove_node_ee: node_ee not existent")


let rec take_first_node_ee (ee : exe_env) : (int * (proc_env list)) =
	match ee with
	| (num_node, node_ee)::res_ee -> (num_node, node_ee)
	| [] -> failwith("Error during take_first_node_ee: node_exe_env not existent")
	

let rec extract_new_process (nnode : int) (ee : exe_env) (ase : abs_iot_stores) : (int * proc_env * exe_env) =
	let node_ee = take_node_ee nnode ee in
	if (node_ee = []) then 
		begin
			let new_ee = remove_node_ee nnode ee in
			if (new_ee = []) then 
				(0, ("", InactProcess, Free), [])
			else begin
				let (num_node_ee, new_node_ee) = take_first_node_ee new_ee in
				let releases = take_abs_releases num_node_ee ase in
				let (exe_proc, exe_node) = extract_node_process num_node_ee new_node_ee releases [] in
				(num_node_ee, exe_proc, (update_node_ee num_node_ee exe_node new_ee))
			end
		end
	else begin
		let releases = take_abs_releases nnode ase in 
		let (exe_proc, exe_node) = extract_node_process nnode node_ee releases [] in
		(nnode, exe_proc, (update_node_ee nnode exe_node ee))
	end


let rec variable_abstract_value (var : string) (vars_ase : (string * (abs_term list)) list)	: abs_term list =
	match vars_ase with
	| (name, abs_value)::res_vars ->
			if (name = var) then abs_value
			else variable_abstract_value var res_vars
	| [] -> failwith("Error: variable_abstract_value operation gone bad")


let rec convert_terms (terms : term) : term list =
	match terms with
	| ParallelTerms(terma, termb) -> (convert_terms terma) @ (convert_terms termb)
	| _ as t -> t::[]


let rec update_cfa_store (var : string) (term : abs_term list) (store : (string * (abs_term list)) list) : (string * (abs_term list)) list =
	match store with
	| (varname, abs_value)::res_vars ->
			if (varname = var) then 
				begin
					let str_term = string_list_of_absterms term in
					let str_abs_values = string_list_of_absterms abs_value in
					let new_abs_value = merge_abs_term_lists str_term term str_abs_values abs_value in
					(var, new_abs_value)::res_vars
				end
			else (varname, abs_value)::(update_cfa_store var term res_vars)
	| [] -> failwith("Error during update_cfa_store")


let rec update_node_var_cfa (var : string) (node_cfa : node_cfa_env) (term : abs_term list) : (node_cfa_env) =
	let store = node_cfa.abs_cfa_store in
	let updated_store = update_cfa_store var term store in
	{
		abs_cfa_store = updated_store;
		abs_net_env = node_cfa.abs_net_env;
		abs_data_coll = node_cfa.abs_data_coll;
		abs_actuator_dec = node_cfa.abs_actuator_dec;
	}

let rec update_ase_store (var : string) (term : abs_term list) (store : (string * (abs_term list)) list) : ((string * (abs_term list)) list) =
	match store with
	| (varname, abs_value)::res_vars ->
		if (varname = var) then (var, term)::res_vars
		else (varname, abs_value)::(update_ase_store var term res_vars)
	| [] -> failwith("Error during update_ase_store")

let rec update_node_var_ase (var : string) (node_ase : abs_store) (term : abs_term list) : (abs_store) =
	let store = node_ase.abs_var in
	let updated_store = update_ase_store var term store in
	{
		abs_var = updated_store;
		abs_input = node_ase.abs_input;
		abs_release = node_ase.abs_release;
	}


let rec add_release_to_ase (nnode : int) (var : string) (ase : abs_iot_stores) : abs_iot_stores =
	let node_ase = take_node_ase nnode ase in
	let new_node_ase =
		{
			abs_var = node_ase.abs_var;
			abs_input = node_ase.abs_input;
			abs_release = var::(node_ase.abs_release);
		} in
	update_ase nnode new_node_ase ase


let rec from_terms_to_vars (term : term) : (string list) =
	match term with
	| ParallelTerms(a, b) -> (from_terms_to_vars a)@(from_terms_to_vars b)
	| Variable(a) -> a::[]
	| _ -> failwith("Error from_terms_to_vars: the term contains a non-variable")


let rec string_of_input (abs_values : (abs_term list) list) : string =
	let string_list = List.map (string_of_absterms) abs_values in
	concatenate_string_list string_list

let rec check_input_presence (new_input : int * string * ((abs_term list) list)) (input_list : (int * string * ((abs_term list) list)) list) : bool =
	match input_list with
	| (src, const, abs_terms)::res_input_list ->
		begin
			match new_input with
			| (new_src, new_const, new_abs_terms) ->
				let abs_terms_str = string_of_input abs_terms in
				let new_abs_terms_str = string_of_input new_abs_terms in
				if ((new_src = src) && (const = new_const) && (abs_terms_str = new_abs_terms_str)) then true
				else check_input_presence new_input res_input_list
		end
	| [] -> false


let rec update_ase_input (new_input : int * string * ((abs_term list) list)) (input_list : (int * string * ((abs_term list) list)) list) : (int * string * ((abs_term list) list)) list =
	new_input::input_list 
			

let rec update_node_ase_input (nnode : int) (new_input : int * string * ((abs_term list) list)) (ase : abs_iot_stores) : abs_iot_stores =
	let node_ase = take_node_ase nnode ase in
	let inp_store = node_ase.abs_input in
	let updated_store = update_ase_input new_input inp_store in
	let new_node_ase =
	{
		abs_var = node_ase.abs_var;
		abs_input = updated_store;
		abs_release = node_ase.abs_release;
	} in 
	update_ase nnode new_node_ase ase


let rec take_abs_terms_of_var (name : string) (abs_vars : (string * (abs_term list)) list) : (abs_term list) =
	match abs_vars with
	| (varname, abs_terms)::res_vars -> 
			if varname = name then abs_terms
			else take_abs_terms_of_var name res_vars
	| [] -> failwith("Error during var taking: var not found")


let rec merge_abs_var (abs_vars1 : (string * (abs_term list)) list) (abs_vars2 : (string * (abs_term list)) list) : (string * (abs_term list)) list =
	match abs_vars1 with
	| (name, abs_terms1)::res_vars ->
			let abs_terms2 = take_abs_terms_of_var name abs_vars2 in
			let str_term1 = string_list_of_absterms abs_terms1 in
			let str_term2 = string_list_of_absterms abs_terms2 in
			let new_abs_terms = merge_abs_term_lists str_term1 abs_terms1 str_term2 abs_terms2 in
			(name, new_abs_terms)::(merge_abs_var res_vars abs_vars2)
	| [] -> []


let rec merge_abs_input (input_list1 : (int * string * ((abs_term list) list)) list) (input_list2 : (int * string * ((abs_term list) list)) list) : (int * string * ((abs_term list) list)) list =
	match input_list1 with
	| input::res_inputs -> 
			if (check_input_presence input input_list2) then 
				merge_abs_input res_inputs input_list2 
			else merge_abs_input res_inputs (input::input_list2)
	| [] -> input_list2


let rec merge_inputs_ase (node_ase1 : abs_store) (node_ase2 : abs_store) : abs_store =	
	{
		abs_var = node_ase1.abs_var;
		abs_input = (merge_abs_input node_ase1.abs_input node_ase2.abs_input);
		abs_release = node_ase1.abs_release;  
	}	


let rec merge_node_ase (node_ase1 : abs_store) (node_ase2 : abs_store) : abs_store =	
	{
		abs_var = (merge_abs_var node_ase1.abs_var node_ase2.abs_var);
		abs_input = (merge_abs_input node_ase1.abs_input node_ase2.abs_input);
		abs_release = node_ase1.abs_release;  
	}	


let rec merge_data_coll (terms1 : abs_term list) (terms2 : abs_term list) : abs_term list =
	let str_term1 = string_list_of_absterms terms1 in
	let str_term2 = string_list_of_absterms terms2 in
	merge_abs_term_lists str_term1 terms1 str_term2 terms2


let rec merge_data_coll_list (nnode : int) (cfaes : cfa_env list) (base_cfae : cfa_env) : cfa_env =
	match cfaes with
	| cfae::res_cfaes -> 
		let node_cfa1 = take_node_cfa nnode base_cfae in
		let node_cfa2 = take_node_cfa nnode cfae in
		let new_node_cfa = 
			{
				abs_cfa_store = node_cfa1.abs_cfa_store;
				abs_net_env = node_cfa1.abs_net_env;
				abs_data_coll = (merge_data_coll node_cfa1.abs_data_coll node_cfa2.abs_data_coll);
				abs_actuator_dec = node_cfa1.abs_actuator_dec;
			} in
		let new_cfae = update_node_cfa nnode new_node_cfa base_cfae in
		merge_data_coll_list nnode res_cfaes new_cfae
	| [] -> base_cfae


let rec merge_input_data_coll (groups : (abs_term list) list) (base_group : (abs_term list)) : abs_term list =
	match groups with
	| frst_list::res_list ->
		let new_base = merge_data_coll frst_list base_group in
		merge_input_data_coll res_list new_base
	| [] -> base_group


let rec output_management (nnode : int) (const : string) (abs_terms : (abs_term list) list) (dest : node_number) (cfae : cfa_env) (ase : abs_iot_stores) : cfa_env * abs_iot_stores =
	match dest with
	| NoNode -> (cfae, ase)
	| ParallelNodeNumber(numa, numb) ->
			let (new_cfae, new_ase) = output_management nnode const abs_terms numa cfae ase in
			output_management nnode const abs_terms numb new_cfae new_ase
	| NodeNumber(num) ->
			let node_cfa = take_node_cfa num cfae in
			let new_node_cfa =
				{
					abs_cfa_store = node_cfa.abs_cfa_store;
					abs_net_env = (nnode, const, abs_terms)::(node_cfa.abs_net_env);
					abs_data_coll = merge_input_data_coll abs_terms node_cfa.abs_data_coll;
					abs_actuator_dec = node_cfa.abs_actuator_dec;
				} in
			let node_ase = take_node_ase num ase in
			let new_node_ase =
				{
					abs_var = node_ase.abs_var;
					abs_input = (nnode, const, abs_terms)::(node_ase.abs_input);
					abs_release = node_ase.abs_release;
				} in
			((update_node_cfa num new_node_cfa cfae),(update_ase num new_node_ase ase)) 
	

let rec input_management (nnode : int) (src : int) (const : string) (vars_list : string list) (counter : int) (cfae : cfa_env) (ase : abs_iot_stores) : (cfa_env * abs_iot_stores) =
	match vars_list with
	| var::res_vars ->
		let node_cfa = take_node_cfa nnode cfae in
		let node_ase = take_node_ase nnode ase in
		let new_input = AbsInput(src, const, counter)::[] in
		let new_node_cfa = update_node_var_cfa var node_cfa new_input in
		let new_node_ase = update_node_var_ase var node_ase new_input in
		let new_cfae = update_node_cfa nnode new_node_cfa cfae in
		let new_ase = update_ase nnode new_node_ase ase in 
		input_management nnode src const res_vars (counter+1) new_cfae new_ase
	| [] -> (cfae, ase)


let rec take_commands (actuator : string) (decision_list : (string * ((string * (abs_condition list)) list)) list) : (string * (abs_condition list)) list =
	match decision_list with
	| (act, commands)::res_actuators ->
			if (act = actuator) then commands
			else take_commands actuator res_actuators
	| [] -> failwith("Error during take_commands: actuator doesn't exist")


let rec take_dependence (cmdname : string) (cmds_list : (string * (abs_condition list)) list) : (abs_condition list) =
	match cmds_list with
	| (cmd, dep)::res_cmds ->
			if (cmd = cmdname) then dep
			else take_dependence cmdname res_cmds
	| [] -> failwith("Error during take_dependence: command not found")


let rec combine_deps (str_list1 : string list) (str_list2 : string list) (deps1 : abs_condition list) (deps2 : abs_condition list) : (abs_condition list) =
	match str_list1 with
	| frst_string::res_strings ->
		begin
			match deps1 with
			| frst_condition::res_conditions ->
				if (check_str_in_list frst_string str_list2) then
					combine_deps res_strings str_list2 res_conditions deps2
				else 
					begin 
						combine_deps res_strings str_list2 res_conditions (frst_condition::deps2)
					end
			| [] -> failwith("Error combine_deps: the conditions are over but not the strings representing them")
		end
	| [] -> deps2

let rec pprint_act_term (terms : abs_term list) : (string) =
	match terms with
	| frst_terms::res_terms ->
		begin
			match frst_terms with
			| AbsUndefined(a) -> "AU(" ^ a ^ ");" ^ (pprint_act_term res_terms)
			| AbsSen(sen,node) -> "AS(" ^ sen ^ "," ^ (string_of_int node) ^ ");" ^ (pprint_act_term res_terms)
			| AbsConst(value) -> "AC(" ^ (pprint_value value) ^ ");" ^ (pprint_act_term res_terms)
			| AbsConstDep(value, a) -> "ACD(" ^ (pprint_value value) ^ ",[" ^ (pprint_act_term a) ^ "]);" ^ (pprint_act_term res_terms)
			| AbsAdd(a,b) -> "AA([" ^ (pprint_act_term a) ^ "][" ^ (pprint_act_term b) ^ "]);" ^ (pprint_act_term res_terms)
			| AbsSub(a,b) -> "AS([" ^ (pprint_act_term a) ^ "][" ^ (pprint_act_term b) ^ "]);" ^ (pprint_act_term res_terms)
			| AbsMul(a,b) -> "AM([" ^ (pprint_act_term a) ^ "][" ^ (pprint_act_term b) ^ "]);" ^ (pprint_act_term res_terms)
			| AbsDiv(a,b) -> "AD([" ^ (pprint_act_term a) ^ "][" ^ (pprint_act_term b) ^ "]);" ^ (pprint_act_term res_terms)
			| AbsInput(src, const, count) -> "AI(" ^ (string_of_int src) ^ const ^ (string_of_int count) ^ ");" ^ (pprint_act_term res_terms)
			| AbsCut(a) -> "ACU(" ^ a ^ ")" ^ (pprint_act_term res_terms)
			| AbsOrigin(src, const, terms) -> "AO(" ^ (string_of_int src) ^ "," ^ const ^ ",[" ^ (pprint_act_term terms) ^ "]);" ^ (pprint_act_term res_terms)
			| AbsFun(name, params) ->
					let str_list = List.map (pprint_act_term) params in
					let compact = pprint_str_list str_list in
					"AF(" ^ compact ^ ")" ^ (pprint_act_term res_terms)
		end
	| [] -> ""

let rec pprint_act_dec (cond : abs_condition) : string =
	match cond with
	| CNone -> "CN"
	| CNot(a) -> "CNot," ^ (pprint_act_dec a)
	| CBool(a) -> "CB|" ^ (string_of_bool a) ^ "|"
	| CAnd(a,b) -> "CA|" ^ (pprint_act_dec a) ^ "?" ^ (pprint_act_dec b) ^ "|"
	| COr(a,b) -> "CO|" ^ (pprint_act_dec a) ^ "?" ^ (pprint_act_dec b) ^ "|"
	| CEqual(a,b) -> "CE|" ^ (pprint_act_term a) ^ "?" ^ (pprint_act_term b) ^ "|"
	| CGreater(a,b) -> "CG|" ^ (pprint_act_term a) ^ "?" ^ (pprint_act_term b) ^ "|"
	| CLower(a,b) -> "CL|" ^ (pprint_act_term a) ^ "?" ^ (pprint_act_term b) ^ "|"
	| CEqGr(a,b) -> "CEG|" ^ (pprint_act_term a) ^ "?" ^ (pprint_act_term b) ^ "|"
	| CEqLw(a,b) -> "CEL|" ^ (pprint_act_term a) ^ "?" ^ (pprint_act_term b) ^ "|"


let rec string_list_of_conditions (deps : abs_condition list) : string list =
	match deps with
	| frst_dep::res_dep ->
		(pprint_act_dec frst_dep)::(string_list_of_conditions res_dep)
	| [] -> []


let rec merge_decisions (deps1 : abs_condition list) (deps2 : abs_condition list) : (abs_condition list) =
	let str_cond_list1 = string_list_of_conditions deps1 in
	let str_cond_list2 = string_list_of_conditions deps2 in
	combine_deps str_cond_list1 str_cond_list2 deps1 deps2

let rec merge_commands_info (cmds1 : (string * (abs_condition list)) list) (cmds2 : (string * (abs_condition list)) list) : (string * (abs_condition list)) list =
	match cmds1 with
	| (cmd_name, dep1)::res_cmds1 ->
			let dep2 = take_dependence cmd_name cmds2 in
			(cmd_name, (merge_decisions dep1 dep2))::(merge_commands_info res_cmds1 cmds2)
	|	[] -> []


let rec merge_actuator_decisions (absdec1 : (string * ((string * (abs_condition list)) list)) list)	(absdec2 : (string * ((string * (abs_condition list)) list)) list) : (string * ((string * (abs_condition list)) list)) list =
	match absdec1 with
	| (actuator, cmds1)::res_actuators ->
			let cmds2 = take_commands actuator absdec2 in
			let new_cmds = merge_commands_info cmds1 cmds2 in
			(actuator, new_cmds)::(merge_actuator_decisions res_actuators absdec2)
	| [] -> []


let merge_inputs_cfa (ncfa1 : node_cfa_env) (ncfa2 : node_cfa_env) : node_cfa_env =
	{
		abs_cfa_store = ncfa1.abs_cfa_store;
		abs_net_env = merge_abs_input ncfa1.abs_net_env ncfa2.abs_net_env;
		abs_data_coll = ncfa1.abs_data_coll;
		abs_actuator_dec = ncfa1.abs_actuator_dec;
	}	


let merge_node_cfa (ncfa1 : node_cfa_env) (ncfa2 : node_cfa_env) : node_cfa_env =
	{
		abs_cfa_store = merge_abs_var ncfa1.abs_cfa_store ncfa2.abs_cfa_store;
		abs_net_env = merge_abs_input ncfa1.abs_net_env ncfa2.abs_net_env;
		abs_data_coll = merge_data_coll ncfa1.abs_data_coll ncfa2.abs_data_coll;
		abs_actuator_dec = merge_actuator_decisions ncfa1.abs_actuator_dec ncfa2.abs_actuator_dec;
	}	


let rec take_inp_nnodes (nnode : int) (cfae : cfa_env) : (int list) =
	match cfae with
	| (num, node_cfa)::res_cfae ->
		if (nnode = num) then take_inp_nnodes nnode res_cfae
		else num::(take_inp_nnodes nnode res_cfae)
	| [] -> []
	
let rec merge_input_information (nnodes : int list) (cfae1 : cfa_env) (cfae2 : cfa_env) (ase1 : abs_iot_stores) (ase2 : abs_iot_stores) : (cfa_env * abs_iot_stores) =
	match nnodes with
	| nnode::res_nnodes ->
			let node_cfa1 = take_node_cfa nnode cfae1 in
			let node_cfa2 = take_node_cfa nnode cfae2 in
			let node_ase1 = take_node_ase nnode ase1 in
			let node_ase2 = take_node_ase nnode ase2 in
			let new_node_cfa = merge_inputs_cfa node_cfa1 node_cfa2 in
			let new_node_ase = merge_inputs_ase node_ase1 node_ase2 in
			let new_cfae1 = update_node_cfa nnode new_node_cfa cfae1 in
			let new_ase1 = update_ase nnode new_node_ase ase1 in
			let new_cfae2 = update_node_cfa nnode new_node_cfa cfae2 in
			let new_ase2 = update_ase nnode new_node_ase ase2 in
			merge_input_information res_nnodes new_cfae1 new_cfae2 new_ase1 new_ase2
	| [] -> (cfae1, ase1)


let rec merge_processes_information (nnode : int) (cfae1 : cfa_env) (cfae2 : cfa_env) (ase1 : abs_iot_stores) (ase2 : abs_iot_stores) : (cfa_env * abs_iot_stores) =
	let node_cfa1 = take_node_cfa nnode cfae1 in
	let node_cfa2 = take_node_cfa nnode cfae2 in
	let node_ase1 = take_node_ase nnode ase1 in
	let node_ase2 = take_node_ase nnode ase2 in
	let new_node_cfa = merge_node_cfa node_cfa1 node_cfa2 in
	let new_node_ase = merge_node_ase node_ase1 node_ase2 in
	let new_cfae1 = update_node_cfa nnode new_node_cfa cfae1 in
	let new_ase1 = update_ase nnode new_node_ase ase1 in
	let new_cfae2 = update_node_cfa nnode new_node_cfa cfae2 in
	let new_ase2 = update_ase nnode new_node_ase ase2 in
	let nnodes = take_inp_nnodes nnode new_cfae1 in
	merge_input_information nnodes new_cfae1 new_cfae2 new_ase1 new_ase2

let rec abs_term_in_abs_term_list (term : abs_term) (group : abs_term list) : bool =
	let str_term = string_of_absterm term in
	let str_group_lst = string_list_of_absterms group in
	check_str_in_list str_term str_group_lst 

let rec cl_abs_term_in_abs_term_list (term : abs_term) (group : abs_term list) : bool =
	let str_term = string_of_absterm term in
	let str_group_lst = string_list_of_absterms group in
	check_str_in_list str_term str_group_lst

let rec update_node_collection (nnode : int) (new_absterm : abs_term) (cfae : cfa_env) : cfa_env =
	let node_cfa = take_node_cfa nnode cfae in
	if (abs_term_in_abs_term_list new_absterm node_cfa.abs_data_coll) then
		let new_node_cfa =
		{
			abs_cfa_store = node_cfa.abs_cfa_store;
			abs_net_env = node_cfa.abs_net_env;
			abs_data_coll = new_absterm::(node_cfa.abs_data_coll);
			abs_actuator_dec = node_cfa.abs_actuator_dec;
		} in
		update_node_cfa nnode new_node_cfa cfae
	else cfae


let rec take_abs_val_list (cfae_and_parval : (cfa_env * abs_term list) list) : (abs_term list) list =
	match cfae_and_parval with
	| (cfae, abs_val)::res_env ->
			abs_val::(take_abs_val_list res_env)
	| [] -> []


let rec take_cfaes (cfae_and_parval : (cfa_env * abs_term list) list) : cfa_env list =
	match cfae_and_parval with
	| (cfae, abs_val)::res_env ->
			cfae::(take_cfaes res_env)
	| [] -> []


let rec link_conditions (conds : abs_condition list) : abs_condition =
	match conds with
	| cond::res_cond ->
			if (res_cond = []) then cond
			else CAnd(cond, (link_conditions res_cond))
	| [] -> failwith("Error link_conditions: the condition list is empty")


let rec update_cmd (action : string) (cond : abs_condition) (commands : (string * (abs_condition list)) list) : (string * (abs_condition list)) list =
	match commands with
	| (cmd, dep)::res_cmds ->
			if (action = cmd) then (action, (cond::dep))::res_cmds
			else (cmd, dep)::(update_cmd action cond res_cmds)
	| [] -> failwith("Error update_cmd: action not found")


let rec update_dep (actloc : string) (action : string) (cond : abs_condition) (act_list : (string * ((string * (abs_condition list)) list)) list) : (string * ((string * (abs_condition list)) list)) list =
	match act_list with
	| (actuator, commands)::res_acts ->
			if (actloc = actuator) then (actloc, (update_cmd action cond commands))::res_acts
			else (actuator, commands)::(update_dep actloc action cond res_acts)
	| [] -> failwith("Error during update_dep: actuator not found") 
	

let rec update_actcmd_dependancy (nnode : int) (actloc : string) (action : string) (cond : abs_condition) (cfae : cfa_env) : (cfa_env) =
	let node_cfa = take_node_cfa nnode cfae in
	let new_node_cfa =
		{
			abs_cfa_store = node_cfa.abs_cfa_store;
			abs_net_env = node_cfa.abs_net_env;
			abs_data_coll = node_cfa.abs_data_coll;
			abs_actuator_dec = (update_dep actloc action cond node_cfa.abs_actuator_dec);
		} in
	update_node_cfa nnode new_node_cfa cfae


let rec insert_waiting_process (nnode : int) (old_proc : proc_env) (ee : exe_env) : exe_env =
	let node_ee = take_node_ee nnode ee in
	let new_node_ee = old_proc::(node_ee) in
	update_node_ee nnode new_node_ee ee


(************************ POST-PROCESSING FUNCTIONS ***************************)
let rec check_valuable_terms (terms : abs_term list) : bool =
	match terms with
	| frst_term::res_terms ->
		begin
			match frst_term with
			| AbsUndefined(_) -> check_valuable_terms res_terms
			| AbsSen(_, _) -> true
			| AbsConst(value) -> check_valuable_terms res_terms
			| AbsFun(_, _) -> true 
			| AbsConstDep(_, _) -> true 
			| AbsAdd(abs_term1, abs_term2) -> 
					if ((check_valuable_terms abs_term1) || (check_valuable_terms abs_term2)) then true
					else check_valuable_terms res_terms
			| AbsSub(abs_term1, abs_term2) -> 
					if ((check_valuable_terms abs_term1) || (check_valuable_terms abs_term2)) then true
					else check_valuable_terms res_terms
			| AbsMul(abs_term1, abs_term2) -> 
					if ((check_valuable_terms abs_term1) || (check_valuable_terms abs_term2)) then true
					else check_valuable_terms res_terms
			| AbsDiv(abs_term1, abs_term2) -> 
					if ((check_valuable_terms abs_term1) || (check_valuable_terms abs_term2)) then true
					else check_valuable_terms res_terms
			| AbsCut(str) -> check_valuable_terms res_terms 
			| AbsOrigin(src, const, terms) -> true
			| AbsInput(src, const, counter) -> true
		end
	| [] -> false


let rec remove_duplicates (terms : abs_term list) : (abs_term list) =
	match terms with
	| frst_term::res_terms ->
		if (abs_term_in_abs_term_list frst_term res_terms) then remove_duplicates res_terms
		else frst_term::(remove_duplicates res_terms)
	| [] -> []


let rec clean_duplicates (terms : abs_term list) : (abs_term list) =
	match terms with
	| frst_term::res_terms ->
		if (cl_abs_term_in_abs_term_list frst_term res_terms) then clean_duplicates res_terms
		else frst_term::(clean_duplicates res_terms)
	| [] -> []


let rec remove_useless_terms (terms : abs_term list) : abs_term list =
	match terms with
	| frst_term::res_terms ->
		begin
			match frst_term with
			| AbsUndefined(_) -> frst_term::(remove_useless_terms res_terms)
			| AbsSen(_, _) -> frst_term::(remove_useless_terms res_terms)
			| AbsConst(_) -> remove_useless_terms res_terms
			| AbsFun(_, _) -> frst_term::(remove_useless_terms res_terms) 
			| AbsConstDep(_, const_absterms) -> const_absterms@(remove_useless_terms res_terms) 
			| AbsAdd(abs_term1, abs_term2) -> 
					if ((check_valuable_terms abs_term1) || (check_valuable_terms abs_term2)) then
						(remove_useless_terms abs_term1)@(remove_useless_terms abs_term2)@(remove_useless_terms res_terms)
					else remove_useless_terms res_terms
			| AbsSub(abs_term1, abs_term2) -> 
					if ((check_valuable_terms abs_term1) || (check_valuable_terms abs_term2)) then
						(remove_useless_terms abs_term1)@(remove_useless_terms abs_term2)@(remove_useless_terms res_terms)
					else remove_useless_terms res_terms
			| AbsMul(abs_term1, abs_term2) -> 
					if ((check_valuable_terms abs_term1) || (check_valuable_terms abs_term2)) then
						(remove_useless_terms abs_term1)@(remove_useless_terms abs_term2)@(remove_useless_terms res_terms)
					else remove_useless_terms res_terms
			| AbsDiv(abs_term1, abs_term2) -> 
					if ((check_valuable_terms abs_term1) || (check_valuable_terms abs_term2)) then
						(remove_useless_terms abs_term1)@(remove_useless_terms abs_term2)@(remove_useless_terms res_terms)
					else remove_useless_terms res_terms
			| AbsCut(_) -> frst_term::(remove_useless_terms res_terms) 
			| AbsOrigin(_, _, _) -> frst_term::(remove_useless_terms res_terms)
			| AbsInput(_, _, _) -> frst_term::(remove_useless_terms res_terms)
		end
	| [] -> []


let rec extract_absterms_from_condition (cond : abs_condition) (base_terms : abs_term list) : (abs_term list) =
	match cond with
	| CNone -> failwith("Error during extract_absterms_from_condition: CNone has no sense")
	| CNot(a) -> extract_absterms_from_condition a base_terms
	| CBool(a) -> base_terms
	| CAnd(a,b) ->
			let new_base = extract_absterms_from_condition a base_terms in
			extract_absterms_from_condition b new_base
	| COr(a,b) ->
			let new_base = extract_absterms_from_condition a base_terms in
			extract_absterms_from_condition b new_base
	| CEqual(a,b) ->
			let str_a_list = string_list_of_absterms a in
			let str_base_terms_list = string_list_of_absterms base_terms in
			let new_base = merge_abs_term_lists str_a_list a str_base_terms_list base_terms in
			let str_b_list = string_list_of_absterms b in
			let str_new_base_list = string_list_of_absterms new_base in
			let new_base2 = merge_abs_term_lists str_b_list b str_new_base_list new_base in
			let new_base3 = remove_useless_terms new_base2 in
			remove_duplicates new_base3
	| CGreater(a,b) ->
			let str_a_list = string_list_of_absterms a in
			let str_base_terms_list = string_list_of_absterms base_terms in
			let new_base = merge_abs_term_lists str_a_list a str_base_terms_list base_terms in
			let str_b_list = string_list_of_absterms b in
			let str_new_base_list = string_list_of_absterms new_base in
			let new_base2 = merge_abs_term_lists str_b_list b str_new_base_list new_base in
			let new_base3 = remove_useless_terms new_base2 in
			remove_duplicates new_base3
	| CLower(a,b) ->
			let str_a_list = string_list_of_absterms a in
			let str_base_terms_list = string_list_of_absterms base_terms in
			let new_base = merge_abs_term_lists str_a_list a str_base_terms_list base_terms in
			let str_b_list = string_list_of_absterms b in
			let str_new_base_list = string_list_of_absterms new_base in
			let new_base2 = merge_abs_term_lists str_b_list b str_new_base_list new_base in
			let new_base3 = remove_useless_terms new_base2 in
			remove_duplicates new_base3
	| CEqGr(a,b) ->
			let str_a_list = string_list_of_absterms a in
			let str_base_terms_list = string_list_of_absterms base_terms in
			let new_base = merge_abs_term_lists str_a_list a str_base_terms_list base_terms in
			let str_b_list = string_list_of_absterms b in
			let str_new_base_list = string_list_of_absterms new_base in
			let new_base2 = merge_abs_term_lists str_b_list b str_new_base_list new_base in
			let new_base3 = remove_useless_terms new_base2 in
			remove_duplicates new_base3
	| CEqLw(a,b) ->
			let str_a_list = string_list_of_absterms a in
			let str_base_terms_list = string_list_of_absterms base_terms in
			let new_base = merge_abs_term_lists str_a_list a str_base_terms_list base_terms in
			let str_b_list = string_list_of_absterms b in
			let str_new_base_list = string_list_of_absterms new_base in
			let new_base2 = merge_abs_term_lists str_b_list b str_new_base_list new_base in
			let new_base3 = remove_useless_terms new_base2 in
			remove_duplicates new_base3
	

let rec extract_undefined_terms (var : string)	(nnode : int) (ase : abs_iot_stores) : abs_term list =
	let node_ase = take_node_ase nnode ase in
	take_abs_terms_of_var var node_ase.abs_var


let rec extend_undefined_terms_terms (nnode : int) (ase : abs_iot_stores) (terms : (abs_term list)) : abs_term list =
	match terms with
	| frst_term::res_terms ->
		begin
			match frst_term with
			| AbsUndefined(a) -> (frst_term::(extract_undefined_terms a nnode ase))@(extend_undefined_terms_terms nnode ase res_terms)
			| AbsSen(sen,node) -> (frst_term)::(extend_undefined_terms_terms nnode ase res_terms)
			| AbsConst(value) -> (frst_term)::(extend_undefined_terms_terms nnode ase res_terms)
			| AbsConstDep(value, abs_term1) -> (AbsConstDep(value, (extend_undefined_terms_terms nnode ase abs_term1)))::(extend_undefined_terms_terms nnode ase res_terms)
			| AbsAdd(a,b) -> AbsAdd((extend_undefined_terms_terms nnode ase a), (extend_undefined_terms_terms nnode ase b))::(extend_undefined_terms_terms nnode ase res_terms)
			| AbsSub(a,b) -> AbsSub((extend_undefined_terms_terms nnode ase a), (extend_undefined_terms_terms nnode ase b))::(extend_undefined_terms_terms nnode ase res_terms)
			| AbsMul(a,b) -> AbsMul((extend_undefined_terms_terms nnode ase a), (extend_undefined_terms_terms nnode ase b))::(extend_undefined_terms_terms nnode ase res_terms)
			| AbsDiv(a,b) -> AbsDiv((extend_undefined_terms_terms nnode ase a), (extend_undefined_terms_terms nnode ase b))::(extend_undefined_terms_terms nnode ase res_terms)
			| AbsInput(src, const, count) -> frst_term::(extend_undefined_terms_terms nnode ase res_terms)
			| AbsCut(a) -> frst_term::(extend_undefined_terms_terms nnode ase res_terms)
			| AbsOrigin(src, const, terms) -> frst_term::(extend_undefined_terms_terms nnode ase res_terms)
			| AbsFun(name, params) -> 
					let abs_terms_list = List.map (extend_undefined_terms_terms nnode ase) params in
					(AbsFun(name, abs_terms_list))::(extend_undefined_terms_terms nnode ase res_terms)
		end
	| [] -> []

let rec extend_undefined_terms_terms_lists (nnode : int) (terms_list : (abs_term list) list) (ase : abs_iot_stores) : (abs_term list) list =
	match terms_list with
	| frst_terms::res_terms ->
			(extend_undefined_terms_terms nnode ase frst_terms)::(extend_undefined_terms_terms_lists nnode res_terms ase)
	| [] -> []


let rec extend_undefined_terms_input (nnode : int) (inputs : (int * string * ((abs_term list) list)) list) (ase : abs_iot_stores) : (int * string * ((abs_term list) list)) list =
	match inputs with
	| (src, const, terms_lists)::res_inputs ->
			(src, const, (extend_undefined_terms_terms_lists src terms_lists ase))::(extend_undefined_terms_input nnode res_inputs ase)
	| [] -> []


let rec extend_undefined_terms_cond (nnode : int) (cond : abs_condition) (ase : abs_iot_stores) : abs_condition =
	match cond with
	| CNone -> cond
	| CNot(a) -> CNot(extend_undefined_terms_cond nnode a ase)
	| CBool(a) -> cond
	| CAnd(a,b) -> CAnd((extend_undefined_terms_cond nnode a ase),(extend_undefined_terms_cond nnode b ase))
	| COr(a,b) -> COr((extend_undefined_terms_cond nnode a ase),(extend_undefined_terms_cond nnode b ase))
	| CEqual(a,b) -> CEqual((extend_undefined_terms_terms nnode ase a),(extend_undefined_terms_terms nnode ase b))
	| CGreater(a,b) -> CGreater((extend_undefined_terms_terms nnode ase a),(extend_undefined_terms_terms nnode ase b))
	| CLower(a,b) -> CLower((extend_undefined_terms_terms nnode ase a),(extend_undefined_terms_terms nnode ase b))
	| CEqGr(a,b) -> CEqGr((extend_undefined_terms_terms nnode ase a),(extend_undefined_terms_terms nnode ase b))
	| CEqLw(a,b) -> CEqLw((extend_undefined_terms_terms nnode ase a),(extend_undefined_terms_terms nnode ase b))



let rec extend_undefined_terms_conds (nnode : int) (conds : abs_condition list) (ase : abs_iot_stores) : abs_condition list =
	match conds with
	| frst_cond::res_conds ->
			(extend_undefined_terms_cond nnode frst_cond ase)::(extend_undefined_terms_conds nnode res_conds ase)
	| [] -> [] 


let rec extend_undefined_terms_cmds (nnode : int) (cmds : (string * (abs_condition list)) list) (ase : abs_iot_stores) : ((string * (abs_condition list)) list) =
	match cmds with
	| (cmd, conds)::res_cmds ->
			(cmd, (extend_undefined_terms_conds nnode conds ase))::(extend_undefined_terms_cmds nnode res_cmds ase)
	| [] -> []


let rec extend_undefined_terms_actuator (nnode : int) (act_dec : (string * ((string * (abs_condition list)) list)) list) (ase : abs_iot_stores) : (string * ((string * (abs_condition list)) list)) list =
	match act_dec with
	| (act, cmds)::res_acts ->
			(act, (extend_undefined_terms_cmds nnode cmds ase))::(extend_undefined_terms_actuator nnode res_acts ase)
	| [] -> []


let rec extend_undefined_terms_node_cfa (nnode : int) (node_cfa : node_cfa_env) (ase : abs_iot_stores) : node_cfa_env =
	{
		abs_cfa_store = node_cfa.abs_cfa_store;
		abs_net_env = extend_undefined_terms_input nnode node_cfa.abs_net_env ase;
		abs_data_coll = node_cfa.abs_data_coll;
		abs_actuator_dec = extend_undefined_terms_actuator nnode node_cfa.abs_actuator_dec ase;
	}	
	
let rec extend_undefined_terms_node_ase (nnode : int) (node_ase : abs_store) (ase : abs_iot_stores) : abs_store =
	{
		abs_var = node_ase.abs_var;
		abs_input = extend_undefined_terms_input nnode node_ase.abs_input ase;
		abs_release = node_ase.abs_release
	}


let rec extend_undefined_terms (nnodes : int list) (cfae : cfa_env) (ase : abs_iot_stores) : (cfa_env * abs_iot_stores) =
	match nnodes with
	| nnode::res_nnodes ->
			let node_cfa = take_node_cfa nnode cfae in
			let node_ase = take_node_ase nnode ase in
			let new_node_cfa = extend_undefined_terms_node_cfa nnode node_cfa ase in
			let new_node_ase = extend_undefined_terms_node_ase nnode node_ase ase in
			let new_cfae = update_node_cfa nnode new_node_cfa cfae in
			let new_ase = update_ase nnode new_node_ase ase in
			extend_undefined_terms res_nnodes new_cfae new_ase
	| [] -> (cfae, ase)


let rec take_nodes_num (cfae : cfa_env) : int list =
	match cfae with
	| (nnode, _)::res_cfae ->
			nnode::(take_nodes_num res_cfae)
	| [] -> []


let rec clean_const_dep_terms_inside (terms : abs_term list) : abs_term list =
	match terms with
	| frst_term::res_terms ->
		begin
			match frst_term with
			| AbsUndefined(_) -> (clean_const_dep_terms_inside res_terms)
			| AbsSen(_, _) -> frst_term::(clean_const_dep_terms_inside res_terms)
			| AbsConst(_) -> clean_const_dep_terms_inside res_terms
			| AbsFun(_, _) -> frst_term::(clean_const_dep_terms_inside res_terms) 
			| AbsConstDep(_, const_absterms) -> clean_duplicates (const_absterms@(clean_const_dep_terms_inside res_terms))
			| AbsAdd(abs_term1, abs_term2) -> 
						((clean_const_dep_terms_inside abs_term1)@(clean_const_dep_terms_inside abs_term2)@(clean_const_dep_terms_inside res_terms))
			| AbsSub(abs_term1, abs_term2) -> 
						((clean_const_dep_terms_inside abs_term1)@(clean_const_dep_terms_inside abs_term2)@(clean_const_dep_terms_inside res_terms))
			| AbsMul(abs_term1, abs_term2) -> 
						((clean_const_dep_terms_inside abs_term1)@(clean_const_dep_terms_inside abs_term2)@(clean_const_dep_terms_inside res_terms))
			| AbsDiv(abs_term1, abs_term2) -> 
						((clean_const_dep_terms_inside abs_term1)@(clean_const_dep_terms_inside abs_term2)@(clean_const_dep_terms_inside res_terms))
			| AbsCut(str) -> (clean_const_dep_terms_inside res_terms) 
			| AbsOrigin(src, const, terms) -> frst_term::(clean_const_dep_terms_inside res_terms)
			| AbsInput(src, const, counter) -> frst_term::(clean_const_dep_terms_inside res_terms)
		end
	| [] -> []	


let rec clean_const_dep_terms_terms (terms : abs_term list) : abs_term list =
	match terms with
	| frst_term::res_terms ->
		begin
			match frst_term with
			| AbsUndefined(_) -> frst_term::(clean_const_dep_terms_terms res_terms)
			| AbsSen(_, _) -> frst_term::(clean_const_dep_terms_terms res_terms)
			| AbsConst(_) -> frst_term::clean_const_dep_terms_terms res_terms
			| AbsFun(_, _) -> frst_term::(clean_const_dep_terms_terms res_terms) 
			| AbsConstDep(var, const_absterms) -> AbsConstDep(var, (clean_duplicates (clean_const_dep_terms_inside const_absterms)))::(clean_const_dep_terms_terms res_terms) 
			| AbsAdd(abs_term1, abs_term2) -> 
					AbsAdd((clean_duplicates (clean_const_dep_terms_terms abs_term1)),(clean_duplicates (clean_const_dep_terms_terms abs_term2)))::(clean_const_dep_terms_terms res_terms)
			| AbsSub(abs_term1, abs_term2) -> 
					AbsSub((clean_duplicates (clean_const_dep_terms_terms abs_term1)),(clean_duplicates (clean_const_dep_terms_terms abs_term2)))::(clean_const_dep_terms_terms res_terms)
			| AbsMul(abs_term1, abs_term2) -> 
					AbsMul((clean_duplicates (clean_const_dep_terms_terms abs_term1)),(clean_duplicates (clean_const_dep_terms_terms abs_term2)))::(clean_const_dep_terms_terms res_terms)
			| AbsDiv(abs_term1, abs_term2) -> 
					AbsDiv((clean_duplicates (clean_const_dep_terms_terms abs_term1)),(clean_duplicates (clean_const_dep_terms_terms abs_term2)))::(clean_const_dep_terms_terms res_terms)
			| AbsCut(str) -> (clean_const_dep_terms_terms res_terms) 
			| AbsOrigin(src, const, terms) -> frst_term::(remove_useless_terms res_terms)
			| AbsInput(src, const, counter) -> frst_term::(remove_useless_terms res_terms)
		end
	| [] -> []

	
let rec clean_const_dep_terms_terms_lists (terms_list : (abs_term list) list) : (abs_term list) list =
	match terms_list with
	| frst_terms::res_terms ->
			(clean_duplicates (clean_const_dep_terms_terms frst_terms))::(clean_const_dep_terms_terms_lists res_terms)
	| [] -> []			
	

let rec clean_const_dep_terms_input (inputs : (int * string * ((abs_term list) list)) list) : (int * string * ((abs_term list) list)) list =
	match inputs with
	| (src, const, terms_lists)::res_inputs ->
			(src, const, (clean_const_dep_terms_terms_lists terms_lists))::(clean_const_dep_terms_input res_inputs)
	| [] -> []

let rec clean_const_dep_terms_cond (cond : abs_condition) : abs_condition =
	match cond with
	| CNone -> cond
	| CNot(a) -> CNot(clean_const_dep_terms_cond a)
	| CBool(a) -> cond
	| CAnd(a,b) -> CAnd((clean_const_dep_terms_cond a),(clean_const_dep_terms_cond b))
	| COr(a,b) -> COr((clean_const_dep_terms_cond a),(clean_const_dep_terms_cond b))
	| CEqual(a,b) -> CEqual((clean_duplicates (clean_const_dep_terms_terms a)),(clean_duplicates (clean_const_dep_terms_terms b)))
	| CGreater(a,b) -> CGreater((clean_duplicates (clean_const_dep_terms_terms a)),(clean_duplicates (clean_const_dep_terms_terms b)))
	| CLower(a,b) -> CLower((clean_duplicates (clean_const_dep_terms_terms a)), (clean_duplicates (clean_const_dep_terms_terms b)))
	| CEqGr(a,b) -> CEqGr((clean_duplicates (clean_const_dep_terms_terms a)), (clean_duplicates (clean_const_dep_terms_terms b)))
	| CEqLw(a,b) -> CEqLw((clean_duplicates (clean_const_dep_terms_terms a)), (clean_duplicates (clean_const_dep_terms_terms b)))


let rec clean_const_dep_terms_conds (conds : abs_condition list) : abs_condition list =
	match conds with
	| frst_cond::res_conds ->
			(clean_const_dep_terms_cond frst_cond)::(clean_const_dep_terms_conds res_conds)
	| [] -> [] 


let rec clean_const_dep_terms_cmds (cmds : (string * (abs_condition list)) list) : ((string * (abs_condition list)) list) =
	match cmds with
	| (cmd, conds)::res_cmds ->
			(cmd, (clean_const_dep_terms_conds conds))::(clean_const_dep_terms_cmds res_cmds)
	| [] -> []


let rec clean_const_dep_terms_actuator (act_dec : (string * ((string * (abs_condition list)) list)) list) : (string * ((string * (abs_condition list)) list)) list =
	match act_dec with
	| (act, cmds)::res_acts ->
			(act, (clean_const_dep_terms_cmds cmds))::(clean_const_dep_terms_actuator res_acts)
	| [] -> []


let rec clean_const_dep_terms_node_cfa (node_cfa : node_cfa_env) : node_cfa_env =
	{
		abs_cfa_store = node_cfa.abs_cfa_store;
		abs_net_env =  clean_const_dep_terms_input node_cfa.abs_net_env;
		abs_data_coll = node_cfa.abs_data_coll;
		abs_actuator_dec = clean_const_dep_terms_actuator node_cfa.abs_actuator_dec;
	}	


let rec clean_const_dep_terms_node_ase (node_ase : abs_store) : abs_store =
	{
		abs_var = node_ase.abs_var;
		abs_input =  clean_const_dep_terms_input node_ase.abs_input;
		abs_release = node_ase.abs_release;
	}	


let rec clean_const_dep_terms (nnodes : int list) (cfae : cfa_env) (ase : abs_iot_stores) : (cfa_env * abs_iot_stores) =
	match nnodes with
	| nnode::res_nnodes ->
		let node_cfa = take_node_cfa nnode cfae in
		let node_ase = take_node_ase nnode ase in
		let new_node_cfa = clean_const_dep_terms_node_cfa node_cfa in
		let new_node_ase = clean_const_dep_terms_node_ase node_ase in
		let new_cfae = update_node_cfa nnode new_node_cfa cfae in
		let new_ase = update_ase nnode new_node_ase ase in
		clean_const_dep_terms res_nnodes new_cfae new_ase
| [] -> (cfae, ase)


let rec merge_input_abs_terms (termslist1 : (abs_term list) list) (termslist2 : (abs_term list) list) : (abs_term list) list =
	match termslist1 with
	| terms1::res_terms1 ->
		begin
			match termslist2 with
			| terms2::res_terms2 -> (remove_duplicates (terms1@terms2))::(merge_input_abs_terms res_terms1 res_terms2)
			| [] -> failwith("Error merge_input_abs_terms: the second input operation have more parameters than the first")
		end
	| [] -> [] 


let rec merge_input_with_input_list (src : int) (dest : int) (const : string) (terms : (abs_term list) list) (base_inputs : (int * int * string * ((abs_term list) list)) list) : (int * int * string * ((abs_term list) list)) list =
	match base_inputs with
	| (in_src, in_dest, in_const, in_abs_terms)::res_inputs ->
			if (src = in_src) && (dest = in_dest) && (const = in_const) then 
				(src, dest, const, (merge_input_abs_terms terms in_abs_terms))::res_inputs
			else (in_src, in_dest, in_const, in_abs_terms)::(merge_input_with_input_list src dest const terms res_inputs)
	| [] -> (src, dest, const, terms)::[]


let rec add_inputs_info (src_node : int) (dest : int) (base_inputs : (int * int * string * ((abs_term list) list)) list) (node_inputs : (int * string * ((abs_term list) list)) list) : (int * int * string * ((abs_term list) list)) list =
	match node_inputs with
	| (src, const, abs_terms)::res_inputs ->
			if (src = src_node) then 
				let new_base_inputs = merge_input_with_input_list src dest const abs_terms base_inputs in
				add_inputs_info src_node dest new_base_inputs res_inputs 
			else add_inputs_info src_node dest base_inputs res_inputs
	| [] -> base_inputs 


let rec create_inputs_struct (src : int) (nnodes : int list) (base_inputs : (int * int * string * ((abs_term list) list)) list) (ase : abs_iot_stores) : ((int * int * string * ((abs_term list) list)) list) =
	match nnodes with
	| frst_nnode::res_nnodes ->
			let node_ase = take_node_ase frst_nnode ase in
			let new_base_inputs = add_inputs_info src frst_nnode base_inputs node_ase.abs_input in
			create_inputs_struct src res_nnodes new_base_inputs ase
	| [] -> base_inputs


let rec take_ref_input_terms (terms_list : (abs_term list) list) (counter : int) : abs_term list =
	match terms_list with
	| frst_terms::res_terms ->
			if (counter = 1) then frst_terms
			else take_ref_input_terms res_terms (counter-1)
	| [] -> failwith("Error take_ref_input_terms: input referred to a parameter number higher than the number of parameters of the input")


let rec take_ref_input (src : int) (dest : int) (const : string) (input_list : (int * int * string * ((abs_term list) list)) list) : (abs_term list) list =
	match input_list with
	| (in_src, in_dest, in_const, in_terms_list)::res_inputs ->
			if (src = in_src) && (dest = in_dest) && (const = in_const) then in_terms_list
			else take_ref_input src dest const res_inputs
	| [] -> failwith("Error take_ref_input: referred input not found inside input list")	


let rec translate_input_to_origin (src : int) (dest : int) (const : string) (counter : int) (input_list : (int * int * string * ((abs_term list) list)) list) : abs_term =
	let sel_input_terms = take_ref_input src dest const input_list in
	AbsOrigin(src, const, (take_ref_input_terms sel_input_terms counter))


let rec update_terms_origin (src : int) (dest : int) (input_list : (int * int * string * ((abs_term list) list)) list) (abs_terms : (abs_term list)) : (abs_term list) =
	match abs_terms with
	| frst_term::res_terms ->
		begin
		match frst_term with
		| AbsUndefined(a) -> frst_term::(update_terms_origin src dest input_list res_terms)
		| AbsSen(sen, num_node) -> frst_term::(update_terms_origin src dest input_list res_terms)
		| AbsConst(value) -> frst_term::(update_terms_origin src dest input_list res_terms)
		| AbsFun(name, params_terms) -> AbsFun(name, (List.map (update_terms_origin src dest input_list) params_terms))::(update_terms_origin src dest input_list res_terms)
		| AbsConstDep(value, abs_term1) -> AbsConstDep(value, (remove_duplicates (update_terms_origin src dest input_list abs_term1)))::(update_terms_origin src dest input_list res_terms)
		| AbsAdd(abs_term1, abs_term2) -> AbsAdd((remove_duplicates (update_terms_origin src dest input_list abs_term1)), (remove_duplicates (update_terms_origin src dest input_list abs_term2)))::(update_terms_origin src dest input_list res_terms)
		| AbsSub(abs_term1, abs_term2) -> AbsSub((remove_duplicates (update_terms_origin src dest input_list abs_term1)), (remove_duplicates (update_terms_origin src dest input_list abs_term2)))::(update_terms_origin src dest input_list res_terms)
		| AbsMul(abs_term1, abs_term2) -> AbsMul((remove_duplicates (update_terms_origin src dest input_list abs_term1)), (remove_duplicates (update_terms_origin src dest input_list abs_term2)))::(update_terms_origin src dest input_list res_terms)
		| AbsDiv(abs_term1, abs_term2) -> AbsDiv((remove_duplicates (update_terms_origin src dest input_list abs_term1)), (remove_duplicates (update_terms_origin src dest input_list abs_term2)))::(update_terms_origin src dest input_list res_terms)
		| AbsCut(str) -> frst_term::(update_terms_origin src dest input_list res_terms)
		| AbsOrigin(or_src, or_const, terms) -> AbsOrigin(or_src, or_const, (update_terms_origin src or_src input_list terms))::(update_terms_origin src dest input_list res_terms)
		| AbsInput(in_src, in_const, counter) -> 
				if (in_src = src) then
					let new_origin = translate_input_to_origin src dest in_const counter input_list in
					new_origin::(update_terms_origin src dest input_list res_terms)
				else frst_term::(update_terms_origin src dest input_list res_terms)
		end
	| [] -> []
	
			
let rec update_terms_list_origin (src : int) (dest : int) (abs_terms : (abs_term list) list) (input_list : (int * int * string * ((abs_term list) list)) list) : (abs_term list) list =
	match abs_terms with
	| frst_list::res_lists->
		(update_terms_origin src dest input_list frst_list)::(update_terms_list_origin src dest res_lists input_list)
	| [] -> []


let rec change_net_input_to_origin (input_src : int) (input_dest : int) (node_inputs : (int * string * ((abs_term list) list)) list) (input_list : (int * int * string * ((abs_term list) list)) list) : (int * string * ((abs_term list) list)) list =
	match node_inputs with
	| (src, const, abs_terms)::res_node_inputs ->
			(src, const, (update_terms_list_origin input_src src abs_terms input_list))::(change_net_input_to_origin input_src input_dest res_node_inputs input_list)
	| [] -> []


let rec update_input_refs (src : int) (nnodes : int list) (input_terms : (int * int * string * ((abs_term list) list)) list) (ase : abs_iot_stores) : abs_iot_stores =
	match nnodes with
	| frst_nnode::res_nnodes ->
			let node_ase = take_node_ase frst_nnode ase in
			let new_node_ase = 
				{
					abs_var = node_ase.abs_var;
					abs_input = change_net_input_to_origin src frst_nnode node_ase.abs_input input_terms;
					abs_release = node_ase.abs_release;
				} in
			let new_ase = update_ase frst_nnode new_node_ase ase in
			update_input_refs src res_nnodes input_terms new_ase
	| [] -> ase  	


let rec update_input_structs_origin (res_nnodes : int list) (nnodes : int list) (ase : abs_iot_stores) : (abs_iot_stores) =
	match res_nnodes with
	| nnode::res_nns ->
		let input_terms = create_inputs_struct nnode nnodes [] ase in
		let new_ase = update_input_refs nnode nnodes input_terms ase in
		update_input_structs_origin res_nns nnodes new_ase
	| [] -> ase


let rec take_sel_terms (terms : (abs_term list) list) (counter : int) : (abs_term list) =
	match terms with
	| frst_term::res_terms ->
			if counter = 1 then frst_term
			else take_sel_terms res_terms (counter-1)
	| [] -> failwith("Error take_sel_terms: the number of parameter that should be taken exceeds the max number of parameters of the input operation")


let rec take_sel_input (src : int) (const : string) (input_list : (int * string * ((abs_term list) list)) list) : ((abs_term list) list) =
	match input_list with
	| (in_src, in_const, terms)::res_inputs ->
		if (src = in_src) && (const = in_const) then terms
		else (take_sel_input src const res_inputs)
	| [] -> failwith("Error take_sel_input: the input with the indicated source and identifier is not found")


let rec from_input_to_origin_terms (dest : int) (ase : abs_iot_stores) (terms : abs_term list) : abs_term list =
	match terms with
	| frst_term::res_terms ->
		begin
		match frst_term with
		| AbsUndefined(a) -> frst_term::(from_input_to_origin_terms dest ase res_terms)
		| AbsSen(sen,node) -> frst_term::(from_input_to_origin_terms dest ase res_terms)
		| AbsConst(value) -> frst_term::(from_input_to_origin_terms dest ase res_terms)
		| AbsConstDep(value, abs_term1) -> AbsConstDep(value, (from_input_to_origin_terms dest ase abs_term1))::(from_input_to_origin_terms dest ase res_terms)
		| AbsAdd(a,b) -> AbsAdd((from_input_to_origin_terms dest ase a), (from_input_to_origin_terms dest ase b))::(from_input_to_origin_terms dest ase res_terms)
		| AbsSub(a,b) -> AbsSub((from_input_to_origin_terms dest ase a), (from_input_to_origin_terms dest ase b))::(from_input_to_origin_terms dest ase res_terms)
		| AbsMul(a,b) -> AbsMul((from_input_to_origin_terms dest ase a), (from_input_to_origin_terms dest ase b))::(from_input_to_origin_terms dest ase res_terms)
		| AbsDiv(a,b) -> AbsDiv((from_input_to_origin_terms dest ase a), (from_input_to_origin_terms dest ase b))::(from_input_to_origin_terms dest ase res_terms)
		| AbsInput(src, const, count) -> 
				let node_ase = take_node_ase dest ase in
				let ref_input = take_sel_input src const node_ase.abs_input in
				let ref_abs_terms = take_sel_terms ref_input count in
				AbsOrigin(src, const, ref_abs_terms)::(from_input_to_origin_terms dest ase res_terms)
		| AbsCut(a) -> frst_term::(from_input_to_origin_terms dest ase res_terms)
		| AbsOrigin(src, const, terms) -> frst_term::(from_input_to_origin_terms dest ase res_terms)
		| AbsFun(name, params) -> AbsFun(name, (List.map (from_input_to_origin_terms dest ase) params))::(from_input_to_origin_terms dest ase res_terms)
		end
	| [] -> []


let rec from_input_to_origin_cond (dest : int) (cond : abs_condition) (ase : abs_iot_stores) : abs_condition =
	match cond with
	| CNone -> cond
	| CNot(a) -> CNot(from_input_to_origin_cond dest a ase)
	| CBool(a) -> cond
	| CAnd(a,b) -> CAnd((from_input_to_origin_cond dest a ase), (from_input_to_origin_cond dest b ase))
	| COr(a,b) -> COr((from_input_to_origin_cond dest a ase), (from_input_to_origin_cond dest b ase))
	| CEqual(a,b) -> CEqual((from_input_to_origin_terms dest ase a), (from_input_to_origin_terms dest ase b))
	| CGreater(a,b) -> CGreater((from_input_to_origin_terms dest ase a), (from_input_to_origin_terms dest ase b))
	| CLower(a,b) -> CLower((from_input_to_origin_terms dest ase a), (from_input_to_origin_terms dest ase b))
	| CEqGr(a,b) -> CEqGr((from_input_to_origin_terms dest ase a), (from_input_to_origin_terms dest ase b))
	| CEqLw(a,b) -> CEqLw((from_input_to_origin_terms dest ase a), (from_input_to_origin_terms dest ase b))



let rec from_input_to_origin_alts (dest : int) (alts : abs_condition list) (ase : abs_iot_stores) : abs_condition list =
	match alts with
	| frst_cond::res_alts ->
			(from_input_to_origin_cond dest frst_cond ase)::(from_input_to_origin_alts dest res_alts ase)
	| [] -> []


let rec from_input_to_origin_cmds (dest : int) (cmds : (string * (abs_condition list)) list) (ase : abs_iot_stores) : (string * (abs_condition list)) list =
	match cmds with
	| (cmd, alternatives)::res_cmds ->
			(cmd, (from_input_to_origin_alts dest alternatives ase))::(from_input_to_origin_cmds dest res_cmds ase)
	| [] -> []


let rec from_input_to_origin_actuators (nnode : int) (actuators : (string * ((string * (abs_condition list)) list)) list)	(ase : abs_iot_stores) : (string * ((string * (abs_condition list)) list)) list =
	match actuators with
	| (actloc, cmds)::res_actuators ->
		(actloc, (from_input_to_origin_cmds nnode cmds ase))::(from_input_to_origin_actuators nnode res_actuators ase)
	| [] -> []


let rec update_actuator_input_origin (nnodes : int list) (cfae : cfa_env) (ase : abs_iot_stores) : cfa_env =
	match nnodes with
	| nnode::res_nnodes ->
			let node_cfa = take_node_cfa nnode cfae in
			let new_node_cfa =
				{
					abs_cfa_store = node_cfa.abs_cfa_store;
					abs_net_env = node_cfa.abs_net_env;
					abs_data_coll = node_cfa.abs_data_coll;
					abs_actuator_dec = (from_input_to_origin_actuators nnode node_cfa.abs_actuator_dec ase);
				} in
			let new_cfae = update_node_cfa nnode new_node_cfa cfae in
			update_actuator_input_origin res_nnodes new_cfae ase
	| [] -> cfae


let rec merge_input_list_values (terms1 : (abs_term list) list) (terms2 : (abs_term list) list) : (abs_term list) list =
	match terms1 with
	| frst_terms1::res_terms1 ->
		begin
			match terms2 with
			| frst_terms2::res_terms2 ->
					(remove_duplicates (frst_terms1@frst_terms2))::(merge_input_list_values res_terms1 res_terms2)
			| [] -> failwith("Error merge_input_list_values: the number of lists of the first element is higher than the number of lists of the second element")
		end
	| [] -> []


let rec merge_input_type_terms (src : int) (const : string) (terms_list : (abs_term list) list) (base_inputs : (int * string * ((abs_term list) list)) list) : (int * string * ((abs_term list) list)) list =
	match base_inputs with
	| (in_src, in_const, in_terms_list)::res_inputs ->
			if (in_src = src) && (const = in_const) then 
				(src, const, (merge_input_list_values terms_list in_terms_list))::res_inputs
			else (in_src, in_const, in_terms_list)::(merge_input_type_terms src const terms_list res_inputs)
	| [] -> failwith("Error during merge_input_type_terms: correspondent input not found") 


let rec check_input_type_presence (src : int) (const : string) (base_inputs : (int * string * ((abs_term list) list)) list) : bool =
	match base_inputs with
	| (in_src, in_const, _)::res_inputs ->
			if (src = in_src) && (const = in_const) then true
			else check_input_type_presence src const res_inputs
	| [] -> false 


let rec compact_inputs_inputs (inputs : (int * string * ((abs_term list) list)) list) (base_inputs : (int * string * ((abs_term list) list)) list) : (int * string * ((abs_term list) list)) list =
	match inputs with
	| (src, const, abs_terms_list)::res_inputs ->
			if (check_input_type_presence src const base_inputs) then
				let new_base_inputs = merge_input_type_terms src const abs_terms_list base_inputs in
				compact_inputs_inputs res_inputs new_base_inputs
			else compact_inputs_inputs res_inputs ((src, const, abs_terms_list)::base_inputs)
	| [] -> base_inputs


let rec compact_inputs (nnodes : int list) (ase : abs_iot_stores) : (abs_iot_stores) =
	match nnodes with
	| nnode::res_nnodes ->
			let node_ase = take_node_ase nnode ase in
			let new_node_ase =
				{
					abs_var = node_ase.abs_var;
					abs_input = (compact_inputs_inputs node_ase.abs_input []);
					abs_release = node_ase.abs_release;
				} in
			let new_ase = update_ase nnode new_node_ase ase in
			compact_inputs res_nnodes new_ase
	| [] -> ase


let rec update_input_origin (nnodes : int list) (cfae : cfa_env) (ase : abs_iot_stores) : cfa_env * abs_iot_stores =
	let new_ase1 = compact_inputs nnodes ase in
	let new_ase2 = update_input_structs_origin nnodes nnodes new_ase1 in
	((update_actuator_input_origin nnodes cfae new_ase2), new_ase2)


(* ******************** EVALUATION FUNCTIONS ******************)
let rec eval_term_condition (nnode : int) (node_ase_vars : (string * (abs_term list)) list) (cfae : cfa_env) (depth : int) (ast : term) : (cfa_env * abs_term list) =
	match ast with
	| Value(value) ->
			let new_abs_term = AbsConst(value) in
			let new_cfae = update_node_collection nnode new_abs_term cfae in
			(new_cfae, new_abs_term::[])
	| SensorLoc(loc) -> 
			let new_abs_term = AbsSen(loc, nnode) in
			let new_cfae = update_node_collection nnode new_abs_term cfae in
			(new_cfae, new_abs_term::[])
	| Variable(name) -> (cfae, (variable_abstract_value name node_ase_vars))
	| Add(terma, termb) ->
			let (new_cfa1, abs_value1) = eval_term_condition nnode node_ase_vars cfae depth terma in
			let (new_cfa2, abs_value2) = eval_term_condition nnode node_ase_vars cfae depth termb in
			let new_abs_term = AbsAdd(abs_value1, abs_value2) in
			let new_cfae = update_node_collection nnode new_abs_term cfae in
			(new_cfae, new_abs_term::[]) 
	| Sub(terma, termb) ->
			let (new_cfa1, abs_value1) = eval_term_condition nnode node_ase_vars cfae depth terma in
			let (new_cfa2, abs_value2) = eval_term_condition nnode node_ase_vars cfae depth termb in
			let new_abs_term = AbsSub(abs_value1, abs_value2) in
			let new_cfae = update_node_collection nnode new_abs_term cfae in
			(new_cfae, new_abs_term::[])
	| Mul(terma, termb) ->
			let (new_cfa1, abs_value1) = eval_term_condition nnode node_ase_vars cfae depth terma in
			let (new_cfa2, abs_value2) = eval_term_condition nnode node_ase_vars cfae depth termb in
			let new_abs_term = AbsMul(abs_value1, abs_value2) in
			let new_cfae = update_node_collection nnode new_abs_term cfae in
			(new_cfae, new_abs_term::[])
	| Div(terma, termb) ->
			let (new_cfa1, abs_value1) = eval_term_condition nnode node_ase_vars cfae depth terma in
			let (new_cfa2, abs_value2) = eval_term_condition nnode node_ase_vars cfae depth termb in
			let new_abs_term = AbsDiv(abs_value1, abs_value2) in
			let new_cfae = update_node_collection nnode new_abs_term cfae in
			(new_cfae, new_abs_term::[])
	| Funct(name, args) ->
			if (depth = 7) then 
				begin
					let new_abs_term = AbsCut(name) in
					(cfae, new_abs_term::[])
				end
			else begin
				let args_list = convert_terms args in
				let cfae_and_param_vals = List.map (eval_term_condition nnode node_ase_vars cfae (depth+1)) args_list in
				let par_abs_val = take_abs_val_list cfae_and_param_vals in
				let cfaes = take_cfaes cfae_and_param_vals in
				let new_cfae = merge_data_coll_list nnode cfaes cfae in
				let new_abs_term = AbsFun(name, par_abs_val) in
				let new_cfae2 = update_node_collection nnode new_abs_term new_cfae in
				(new_cfae2, new_abs_term::[])
			end
	| ParallelTerms(terma, termb) -> 
			failwith("Error: used parallel terms in cases where it is not possible to use them")


let rec eval_term (nnode : int) (node_ase_vars : (string * (abs_term list)) list) (cfae : cfa_env) (depth : int) (cond : abs_condition list) (ast : term) : (cfa_env * abs_term list) =
	match ast with
	| Value(value) ->
			if (cond = []) then
				begin
					let new_abs_term = AbsConst(value) in
					let new_cfae = update_node_collection nnode new_abs_term cfae in
					(new_cfae, new_abs_term::[])
				end
			else begin
				let comp_cond = link_conditions cond in
				let abs_terms = extract_absterms_from_condition comp_cond [] in
				let new_abs_term = AbsConstDep(value, abs_terms) in
				let new_cfae = update_node_collection nnode new_abs_term cfae in
				(new_cfae, new_abs_term::[])
			end
	| SensorLoc(loc) -> 
			let new_abs_term = AbsSen(loc, nnode) in
			let new_cfae = update_node_collection nnode new_abs_term cfae in
			(new_cfae, new_abs_term::[])
	| Variable(name) -> (cfae, (variable_abstract_value name node_ase_vars))
	| Add(terma, termb) ->
			let (new_cfa1, abs_value1) = eval_term nnode node_ase_vars cfae depth cond terma in
			let (new_cfa2, abs_value2) = eval_term nnode node_ase_vars cfae depth cond termb in
			let new_abs_term = AbsAdd(abs_value1, abs_value2) in
			let new_cfae = update_node_collection nnode new_abs_term cfae in
			(new_cfae, new_abs_term::[]) 
	| Sub(terma, termb) ->
			let (new_cfa1, abs_value1) = eval_term nnode node_ase_vars cfae depth cond terma in
			let (new_cfa2, abs_value2) = eval_term nnode node_ase_vars cfae depth cond termb in
			let new_abs_term = AbsSub(abs_value1, abs_value2) in
			let new_cfae = update_node_collection nnode new_abs_term cfae in
			(new_cfae, new_abs_term::[])
	| Mul(terma, termb) ->
			let (new_cfa1, abs_value1) = eval_term nnode node_ase_vars cfae depth cond terma in
			let (new_cfa2, abs_value2) = eval_term nnode node_ase_vars cfae depth cond termb in
			let new_abs_term = AbsMul(abs_value1, abs_value2) in
			let new_cfae = update_node_collection nnode new_abs_term cfae in
			(new_cfae, new_abs_term::[])
	| Div(terma, termb) ->
			let (new_cfa1, abs_value1) = eval_term nnode node_ase_vars cfae depth cond terma in
			let (new_cfa2, abs_value2) = eval_term nnode node_ase_vars cfae depth cond termb in
			let new_abs_term = AbsDiv(abs_value1, abs_value2) in
			let new_cfae = update_node_collection nnode new_abs_term cfae in
			(new_cfae, new_abs_term::[])
	| Funct(name, args) ->
			if (depth = 7) then 
				begin
					let new_abs_term = AbsCut(name) in
					(cfae, new_abs_term::[])
				end
			else begin
				let args_list = convert_terms args in
				let cfae_and_param_vals = List.map (eval_term nnode node_ase_vars cfae (depth+1) cond) args_list in
				let par_abs_val = take_abs_val_list cfae_and_param_vals in
				let cfaes = take_cfaes cfae_and_param_vals in
				let new_cfae = merge_data_coll_list nnode cfaes cfae in
				let new_abs_term = AbsFun(name, par_abs_val) in
				let new_cfae2 = update_node_collection nnode new_abs_term new_cfae in
				(new_cfae2, new_abs_term::[])
			end
	| ParallelTerms(terma, termb) -> 
			failwith("Error: used parallel terms in cases where it is not possible to use them")


let rec eval_condition (nnode : int) (ast : condition) (cfae : cfa_env) (ase : abs_iot_stores) : (cfa_env * abs_condition) =
	match ast with
	| Bool(b) -> (cfae, CBool(b))
	| Greater(x, y) ->
			let node_ase = take_node_ase nnode ase in 
			let (new_cfae1, abs_x) = eval_term_condition nnode node_ase.abs_var cfae 0 x in
			let (new_cfae2, abs_y) = eval_term_condition nnode node_ase.abs_var new_cfae1 0 y in
			(new_cfae2, CGreater(abs_x, abs_y))
	| Lower(x, y) -> 
			let node_ase = take_node_ase nnode ase in 
			let (new_cfae1, abs_x) = eval_term_condition nnode node_ase.abs_var cfae 0 x in
			let (new_cfae2, abs_y) = eval_term_condition nnode node_ase.abs_var new_cfae1 0 y in
			(new_cfae2, CLower(abs_x, abs_y))
	| Equal(x, y) ->
			let node_ase = take_node_ase nnode ase in 
			let (new_cfae1, abs_x) = eval_term_condition nnode node_ase.abs_var cfae 0 x in
			let (new_cfae2, abs_y) = eval_term_condition nnode node_ase.abs_var new_cfae1 0 y in
			(new_cfae2, CEqual(abs_x, abs_y))
	| EqGr(x, y) ->
			let node_ase = take_node_ase nnode ase in 
			let (new_cfae1, abs_x) = eval_term_condition nnode node_ase.abs_var cfae 0 x in
			let (new_cfae2, abs_y) = eval_term_condition nnode node_ase.abs_var new_cfae1 0 y in
			(new_cfae2, CEqGr(abs_x, abs_y))
	| EqLw(x, y) ->
			let node_ase = take_node_ase nnode ase in 
			let (new_cfae1, abs_x) = eval_term_condition nnode node_ase.abs_var cfae 0 x in
			let (new_cfae2, abs_y) = eval_term_condition nnode node_ase.abs_var new_cfae1 0 y in
			(new_cfae2, CEqLw(abs_x, abs_y))
	| And(x, y) ->
			let (new_cfae1, cond1) = eval_condition nnode x cfae ase in
			let (new_cfae2, cond2) = eval_condition nnode y new_cfae1 ase in
			(new_cfae2, CAnd(cond1, cond2))
	| Or(x, y) ->
			let (new_cfae1, cond1) = eval_condition nnode x cfae ase in
			let (new_cfae2, cond2) = eval_condition nnode y new_cfae1 ase in
			(new_cfae2, COr(cond1, cond2))


let rec eval_process (name : string) (nnode : int) (cfae : cfa_env) (ase : abs_iot_stores) (ee : exe_env) (cond : abs_condition list) (ast : process) : (cfa_env * abs_iot_stores) =
	match ast with
	| InactProcess -> 
			if (cond = []) then
			begin
				let (num_node, proc_env, new_ee) = extract_new_process nnode ee ase in
				if (num_node = 0) then 
				(cfae, ase)
				else begin
					match proc_env with
					| (pro_name, proc, _) -> eval_process pro_name num_node cfae ase new_ee [] proc
				end 
			end
			else (cfae, ase) 
	| MultiOutput(const, outputs, dest, res_proc) ->
			let terms_list = convert_terms outputs in
			let node_ase = take_node_ase nnode ase in
			let cfae_and_abs_terms = List.map (eval_term nnode node_ase.abs_var cfae 0 cond) terms_list in
			let abs_terms = take_abs_val_list cfae_and_abs_terms in
			let cfaes = take_cfaes cfae_and_abs_terms in
			let new_cfae = merge_data_coll_list nnode cfaes cfae in
			let (new_cfae2, new_ase) = output_management nnode const abs_terms dest new_cfae ase in
			eval_process name nnode new_cfae2 new_ase ee cond res_proc  
	| InputProc(src_node, const, input_vars, res_proc) ->
			let vars_list = from_terms_to_vars input_vars in
			let (new_cfae, new_ase) = input_management nnode src_node const vars_list 1 cfae ase in			
			eval_process name nnode cfae new_ase ee cond res_proc
	| ConditionProc(condition, proca, procb, res_proc) ->
			let (new_cfae0, abs_cond) = eval_condition nnode condition cfae ase in
			let (new_cfae1, new_ase1) = eval_process name nnode new_cfae0 ase ee (abs_cond::cond) proca in
			let (new_cfae2, new_ase2) = eval_process name nnode new_cfae0 ase ee (CNot(abs_cond)::cond) procb in
			let (new_cfae3, new_ase3) = merge_processes_information nnode new_cfae1 new_cfae2 new_ase1 new_ase2 in
			eval_process name nnode new_cfae3 new_ase3 ee cond res_proc 
	| Assignment(x, y, res_proc) ->
			let node_cfae = take_node_cfa nnode cfae in
			let node_ase = take_node_ase nnode ase in
			let (new_cfae0, y1) = eval_term nnode node_ase.abs_var cfae 0 cond y in
			let new_node_cfae = update_node_var_cfa x node_cfae y1 in
			let new_node_ase = update_node_var_ase x node_ase y1 in  
			let new_cfae = update_node_cfa nnode new_node_cfae new_cfae0 in
			let new_ase = update_ase nnode new_node_ase ase in			
			eval_process name nnode new_cfae new_ase ee cond res_proc
	| ActivateActuator(actloc, action, res_proc) ->
			let new_cond = link_conditions cond in
			let new_cfae = update_actcmd_dependancy nnode actloc action new_cond cfae in			
			eval_process name nnode new_cfae ase ee cond res_proc
	| PCloseIter ->
			if (cond = []) then
			begin
				let (num_node, proc_env, new_ee) = extract_new_process nnode ee ase in
				if (num_node = 0) then 
				(cfae, ase)
				else begin
					match proc_env with
					| (pro_name, proc, _) -> eval_process pro_name num_node cfae ase new_ee [] proc
				end
			end
			else (cfae, ase)
	| POpenIter(res_proc) ->
			eval_process name nnode cfae ase ee cond res_proc
	| Wait(var, res_proc) ->
			let rels = take_abs_releases nnode ase in
			if (check_release_happened var rels) then eval_process name nnode cfae ase ee cond res_proc
			else begin
				let releases = take_abs_releases nnode ase in
				let (new_proc, new_ee) = extract_process nnode ee releases in
				let new_ee2 = insert_waiting_process nnode (name, res_proc, StopWait(var)) new_ee in
				begin
					match new_proc with
					| (new_name, new_proc, _) -> eval_process new_name nnode cfae ase new_ee2 [] new_proc
				end
			end
	| Used(var, res_proc) ->
			eval_process name nnode cfae ase ee cond res_proc
	| Prepare(var, res_proc) ->
			eval_process name nnode cfae ase ee cond res_proc
	| Release(var, res_proc) ->
			let new_ase = add_release_to_ase nnode var ase in
			eval_process name nnode cfae new_ase ee cond res_proc
	| Encrypt(var, res_proc) -> 
			eval_process name nnode cfae ase ee cond res_proc
	| Decrypt(var, res_proc) ->
			eval_process name nnode cfae ase ee cond res_proc


let rec eval_IoTStructure (cfae : cfa_env) (ase : abs_iot_stores) (ee : exe_env) : (cfa_env * abs_iot_stores * cfa_env * abs_iot_stores) = 
		let abs_releases = take_abs_releases 1 ase in
		let (exe_proc, new_ee)= extract_process 1 ee abs_releases in 
		match exe_proc with
		| (name, ast, _) ->
			let (cfae_stats, ase_stats) = eval_process name 1 cfae ase new_ee [] ast in
			let _ = pprint_actuator_stats cfae_stats in
			let _ = pprint_input_stats cfae_stats in
			let nnodes = take_nodes_num cfae_stats in
			let (new_cfae, new_ase) = extend_undefined_terms nnodes cfae_stats ase_stats in
			let (new_cfae1, new_ase1) = clean_const_dep_terms nnodes new_cfae new_ase in
			let _ = pprint_ext_actuator_stats new_cfae1 in
			let (new_cfae2, new_ase2) = update_input_origin nnodes new_cfae1 new_ase1 in
			let _ = pprint_final_actuator_stats new_cfae2 in
			(*let (new_cfae, new_ase) = translate_inputs in*)
			(new_cfae1, new_ase1, new_cfae2, new_ase2)



(* ************************* ATTACKS EVALUATOR **********************)
let rec split_sen_nnode (input : string) : bool * string * int =
	if (input = "attack") then (true, "attack", 1)
	else if (input = "exit") then (true, "exit", 1) 
	else begin
		let sen_and_node = Str.split (Str.regexp "-") input in
		let node = List.hd sen_and_node in
		let sen = List.nth sen_and_node 1 in
		if (not (String.starts_with ~prefix:"S" sen)) then
			begin
				let _ = Printf.printf "\nThe provided sensor doesn't starts with 'S'. Retry" in
				(false, "", 1)
			end
		else (true, sen, (int_of_string node))
	end


let rec list_condition (bool_list : bool list) : bool =
	match bool_list with
	| frst_bool::res_bools -> if (frst_bool = true) then true else list_condition res_bools
	| [] -> false


let rec term_sensor_or_input_dependant (terms : abs_term list) : bool =
	match terms with
	| frst_term::res_terms ->
		begin
			match frst_term with
			| AbsUndefined(a) -> term_sensor_or_input_dependant res_terms
			| AbsSen(sen,node) -> true
			| AbsConst(value) -> term_sensor_or_input_dependant res_terms
			| AbsConstDep(value, abs_term1) -> true
			| AbsAdd(a,b) -> (term_sensor_or_input_dependant a) || (term_sensor_or_input_dependant b) || (term_sensor_or_input_dependant res_terms)
			| AbsSub(a,b) -> (term_sensor_or_input_dependant a) || (term_sensor_or_input_dependant b) || (term_sensor_or_input_dependant res_terms)
			| AbsMul(a,b) -> (term_sensor_or_input_dependant a) || (term_sensor_or_input_dependant b) || (term_sensor_or_input_dependant res_terms)
			| AbsDiv(a,b) -> (term_sensor_or_input_dependant a) || (term_sensor_or_input_dependant b) || (term_sensor_or_input_dependant res_terms)
			| AbsInput(src, const, count) -> true
			| AbsCut(a) -> term_sensor_or_input_dependant res_terms
			| AbsOrigin(src, const, terms) -> true
			| AbsFun(name, params) -> 
				let bool_list = List.map (term_sensor_or_input_dependant) params in
				let res = list_condition bool_list in
				if (res = true) then true
				else term_sensor_or_input_dependant res_terms
		end
	| [] -> false


let rec insert_senloc_in_deps (senloc : string) (nnode : int) (base_deps : (string * int) list) : (string * int) list =
	match base_deps with
	| (b_senloc, b_nnode)::res_base ->
			if (b_senloc = senloc) && (b_nnode = nnode) then res_base
			else (b_senloc, b_nnode)::(insert_senloc_in_deps senloc nnode res_base)
	| [] -> (senloc, nnode)::[]


let rec merge_dependancies (deps : (string * int) list) (base_deps : (string * int) list) : (string * int) list =
	match deps with
	| (senloc, nnode)::res_deps ->
			let new_base = insert_senloc_in_deps senloc nnode base_deps in
			merge_dependancies res_deps new_base
	| [] -> base_deps


let rec merge_dependancies_list (deps_list : ((string * int) list) list) (base_deps : (string * int) list) : (string * int) list =
	match deps_list with
	| frst_list::res_lists ->
		let new_base_deps = merge_dependancies frst_list base_deps in
		merge_dependancies_list res_lists new_base_deps
	| [] -> base_deps


let rec check_sen_in_sen_list (sen : string) (nnode : int) (sensors : (string * int) list) : bool =
	match sensors with
	| (senloc, num_node)::res_sensors ->
		if (senloc = sen) && (nnode = num_node) then true
		else check_sen_in_sen_list sen nnode res_sensors
	| [] -> false


let rec take_dependencies (sensors : (string * int) list) (deps : (string * int) list) (terms : abs_term list) : (string * int) list =
	match terms with
	| frst_term::res_terms ->
		begin
			match frst_term with
			| AbsUndefined(a) -> take_dependencies sensors deps res_terms
			| AbsSen(sen,node) -> 
					if (check_sen_in_sen_list sen node sensors) && (not (check_sen_in_sen_list sen node deps)) then
						let new_dep = (sen, node) in
						take_dependencies sensors (new_dep::deps) res_terms
					else take_dependencies sensors deps res_terms
			| AbsConst(value) -> take_dependencies sensors deps res_terms
			| AbsConstDep(value, abs_term1) -> 
					let new_deps = take_dependencies sensors deps abs_term1 in
					take_dependencies sensors new_deps res_terms
			| AbsAdd(a,b) -> 
					let new_deps = take_dependencies sensors deps a in
					let new_deps1 = take_dependencies sensors new_deps b in
					take_dependencies sensors new_deps1 res_terms
			| AbsSub(a,b) -> 
					let new_deps = take_dependencies sensors deps a in
					let new_deps1 = take_dependencies sensors new_deps b in
					take_dependencies sensors new_deps1 res_terms
			| AbsMul(a,b) ->
					let new_deps = take_dependencies sensors deps a in
					let new_deps1 = take_dependencies sensors new_deps b in
					take_dependencies sensors new_deps1 res_terms
			| AbsDiv(a,b) -> 
					let new_deps = take_dependencies sensors deps a in
					let new_deps1 = take_dependencies sensors new_deps b in
					take_dependencies sensors new_deps1 res_terms
			| AbsInput(src, const, count) -> take_dependencies sensors deps res_terms
			| AbsCut(a) -> take_dependencies sensors deps res_terms
			| AbsOrigin(src, const, terms) -> 
					let new_deps = take_dependencies sensors deps terms in
					take_dependencies sensors new_deps res_terms
			| AbsFun(name, params) -> 
					let new_deps_list = List.map (take_dependencies sensors deps) params in
					let new_merged_deps = merge_dependancies_list new_deps_list deps in
					take_dependencies sensors new_merged_deps res_terms 
		end
	| [] -> deps


let rec pprint_attack_sensors (deps : (string * int) list) : string =
	match deps with
	| (senloc, nnode)::res_sens ->
			if (res_sens = []) then "Node " ^ (string_of_int nnode) ^ " sensor " ^ senloc
			else "Node " ^ (string_of_int nnode) ^ " sensor " ^ senloc ^ "; " ^ pprint_attack_sensors res_sens
	| [] -> ""

	
let rec pprint_attack_absterms (sensors : (string * int) list) (terms : abs_term list) : string =
	if (term_sensor_or_input_dependant terms) then 
		begin
		let deps = take_dependencies sensors [] terms in
		if (deps = []) then "Term"
		else "TermDependantFrom[" ^ (pprint_attack_sensors deps) ^ "]"
		end
	else "Constant"



let rec pprint_attack_condition (sensors : (string * int) list) (cond : abs_condition) : string =
	match cond with
	| CNone -> "CNone"
	| CNot(a) -> "NOT(" ^ (pprint_attack_condition sensors a) ^ ")"
	| CBool(a) -> "BOOL(" ^ (string_of_bool a) ^ ")"
	| CAnd(a,b) -> "(" ^ (pprint_attack_condition sensors a) ^ ") AND (" ^ (pprint_attack_condition sensors b) ^ ")"
	| COr(a,b) -> "(" ^ (pprint_attack_condition sensors a) ^ ") OR (" ^ (pprint_attack_condition sensors b) ^ ")"
	| CEqual(a,b) -> "([" ^ (pprint_attack_absterms sensors a) ^ "]) EQUAL TO ([" ^ (pprint_attack_absterms sensors b) ^ "])"
	| CGreater(a,b) -> "([" ^ (pprint_attack_absterms sensors a) ^ "]) GREATER THAN ([" ^ (pprint_attack_absterms sensors b) ^ "])"
	| CLower(a,b) -> "([" ^ (pprint_attack_absterms sensors a) ^ "]) LOWER THAN ([" ^ (pprint_attack_absterms sensors b) ^ "])"
	| CEqGr(a,b) -> "([" ^ (pprint_attack_absterms sensors a) ^ "]) GREATER THAN OR EQUAL TO ([" ^ (pprint_attack_absterms sensors b) ^ "])"
	| CEqLw(a,b) -> "([" ^ (pprint_attack_absterms sensors a) ^ "]) LOWER THAN OR EQUAL TO ([" ^ (pprint_attack_absterms sensors b) ^ "])"


let rec check_possible_attack_terms (sensors : (string * int) list) (terms : abs_term list) : bool =
	match terms with
	| frst_term::res_terms ->
		begin
			match frst_term with
			| AbsUndefined(a) -> check_possible_attack_terms sensors res_terms
			| AbsSen(sen,node) -> 
					if (check_sen_in_sen_list sen node sensors) then true
					else check_possible_attack_terms sensors res_terms
			| AbsConst(value) -> check_possible_attack_terms sensors res_terms
			| AbsConstDep(value, abs_term1) -> 
					(check_possible_attack_terms sensors abs_term1) || (check_possible_attack_terms sensors res_terms)
			| AbsAdd(a,b) -> 
					(check_possible_attack_terms sensors a) || (check_possible_attack_terms sensors b) || (check_possible_attack_terms sensors res_terms)
			| AbsSub(a,b) -> 
					(check_possible_attack_terms sensors a) || (check_possible_attack_terms sensors b) || (check_possible_attack_terms sensors res_terms)
			| AbsMul(a,b) ->
					(check_possible_attack_terms sensors a) || (check_possible_attack_terms sensors b) || (check_possible_attack_terms sensors res_terms)
			| AbsDiv(a,b) -> 
					(check_possible_attack_terms sensors a) || (check_possible_attack_terms sensors b) || (check_possible_attack_terms sensors res_terms)
			| AbsInput(src, const, count) -> check_possible_attack_terms sensors res_terms
			| AbsCut(a) -> check_possible_attack_terms sensors res_terms
			| AbsOrigin(src, const, terms) -> 
					(check_possible_attack_terms sensors terms) || (check_possible_attack_terms sensors res_terms)
			| AbsFun(name, params) -> 
					let bool_list = List.map (check_possible_attack_terms sensors) params in
					let res = list_condition bool_list in
					if (res = true) then true
					else check_possible_attack_terms sensors res_terms
		end
	| [] -> false


let rec check_possible_attack (sensors : (string * int) list) (cond : abs_condition) : (bool) =
	match cond with
	| CNone -> false
	| CNot(a) -> check_possible_attack sensors a
	| CBool(a) -> false
	| CAnd(a,b) -> (check_possible_attack sensors a) || (check_possible_attack sensors b) 
	| COr(a,b) -> (check_possible_attack sensors a) || (check_possible_attack sensors b) 
	| CEqual(a,b) -> (check_possible_attack_terms sensors a) || (check_possible_attack_terms sensors b) 
	| CGreater(a,b) -> (check_possible_attack_terms sensors a) || (check_possible_attack_terms sensors b) 
	| CLower(a,b) -> (check_possible_attack_terms sensors a) || (check_possible_attack_terms sensors b) 
	| CEqGr(a,b) -> (check_possible_attack_terms sensors a) || (check_possible_attack_terms sensors b) 
	| CEqLw(a,b) -> (check_possible_attack_terms sensors a) || (check_possible_attack_terms sensors b) 


let rec pprint_attack_alternative_condition (sensors : (string * int) list) (conds : abs_condition list) (counter : int)	: string =
	match conds with
	| frst_cond::res_conds ->
		if (check_possible_attack sensors frst_cond) then
		"Alternative " ^ (string_of_int counter) ^ ": " ^ (pprint_attack_condition sensors frst_cond) ^ "\n" ^ (pprint_attack_alternative_condition sensors res_conds (counter+1))
		else pprint_attack_alternative_condition sensors res_conds counter
	| [] -> ""


let rec pprint_attack_commands (sensors : (string * int) list) (cmds : ((string * (abs_condition list)) list)) : string =
	match cmds with
	| (frst_cmd, cond)::res_cmds ->
			 ("\nCommand " ^ frst_cmd ^ ": \n" ^ (pprint_attack_alternative_condition sensors cond 1) ^ "\n") ^ (pprint_attack_commands sensors res_cmds)
	| [] -> ""


let rec pprint_attack_actuator (sensors : (string * int) list) (actuators : (string * ((string * (abs_condition list)) list)) list)	: string =
	match actuators with
	| (act, cmds)::res_act -> "\n\n---------------------\n Actuator " ^ act ^ ":\n" ^ (pprint_attack_commands sensors cmds) ^ (pprint_attack_actuator sensors res_act)
	| [] -> ""


let rec complex_attacks_analysis (sensors : (string * int) list) (cfae : cfa_env) (num_attack : int) : unit =
	match cfae with
	| node_cfa::res_cfae -> 
		begin
			match node_cfa with
			| (nnode, cfa_env) ->
				let path = "./attacks/complexAttack/attackResults" ^ (string_of_int num_attack) in
				let oc = open_out_gen [Open_append; Open_creat] 0o666 path in
				let _ = Printf.fprintf oc "%s" ("\n*************************\n\nActuator Attack Node " ^ (string_of_int nnode) ^ ":\n") in
				let _ = Printf.fprintf oc "%s" (pprint_attack_actuator sensors cfa_env.abs_actuator_dec) in
				complex_attacks_analysis sensors res_cfae num_attack
		end
	| [] -> ()


let rec simple_attacks_analysis (sensors : (string * int) list) (cfae : cfa_env) (num_attack : int) : unit =
	match cfae with
	| node_cfa::res_cfae -> 
		begin
			match node_cfa with
			| (nnode, cfa_env) ->
				let path = "./attacks/simpleAttack/attackResults" ^ (string_of_int num_attack) in
				let oc = open_out_gen [Open_append; Open_creat] 0o666 path in
				let _ = Printf.fprintf oc "%s" ("\n*************************\n\nActuator Attack Node " ^ (string_of_int nnode) ^ ":\n") in
				let _ = Printf.fprintf oc "%s" (pprint_attack_actuator sensors cfa_env.abs_actuator_dec) in
				simple_attacks_analysis sensors res_cfae num_attack
		end
	| [] -> ()


let rec analyze_attacks (sensors : (string * int) list) (simple_cfae : cfa_env) (complex_cfae : cfa_env) (num_attack : int) : (bool) =
	let _ = simple_attacks_analysis sensors simple_cfae num_attack in
	let _ = complex_attacks_analysis sensors complex_cfae num_attack in
	true	


let rec update_sensor_list (senloc : string) (nnode : int) (sensors : (string * int) list) : (string * int) list =
	(senloc, nnode)::(sensors)

let rec sensor_insertion (simple_cfae : cfa_env) (complex_cfae : cfa_env) (sensors : (string * int) list) (num_attack : int) (ic) : unit =
	(*let _ = Printf.printf "\nProvide a tampered sensor with a format NODENUMBER-SENSORNAME or submit 'end' to start the analysis: %!" in *)
	let input = In_channel.input_line ic in
	match input with
		| Some(inp) ->
				let (res, senloc, nnode) = split_sen_nnode inp in
				if (res = true && senloc = "exit") then () 
				else if (res = true && senloc = "attack") then
				begin
					let res = analyze_attacks sensors simple_cfae complex_cfae num_attack in 
					if (res = true) then
						let _ = Printf.printf "\nAnalysis managed correctly. Information stored on file attackResult%i\n%!" (num_attack) in
						sensor_insertion simple_cfae complex_cfae [] (num_attack+1) ic
					else ()
				end
				else if (res = false) then sensor_insertion simple_cfae complex_cfae sensors num_attack ic
				else begin
					let new_sensors = update_sensor_list senloc nnode sensors in
					sensor_insertion simple_cfae complex_cfae new_sensors num_attack ic
				end
		| None -> ()


let rec eval_attacks (simple_cfae : cfa_env) (complex_cfae : cfa_env) : unit =
	let path = "./attack.txt" in
	let ic = In_channel.open_text path in
	let _ = Printf.printf "\n\n\n\n\t\t CFA SURFING\nWelcome to the CFA Analysis for Surfing Programs!\n%!" in
	sensor_insertion simple_cfae complex_cfae [] 1 ic





(* ********************** STATIC ANALYSIS FOR THE CONFIGURATION ****************)
let rec update_act_commands (loc : string) (commands : (string list)) (node_actuators : (string * ((string * (abs_condition list)) list)) list) : (string * ((string * (abs_condition list)) list)) list =
	match node_actuators with
	| (aloc, actions)::res_actuators ->
			if (aloc = loc) then (aloc, (add_commands_to_act commands))::res_actuators
			else (aloc, actions)::(update_act_commands loc commands res_actuators)
	| [] -> failwith("Error during updating of the possible commands. Actuator not existent")

let rec frst_eval_actuator (loc : string) (nnode : int) (cfae : cfa_env) (ase : abs_iot_stores) (ee : exe_env) (ast : actuator) : (cfa_env* abs_iot_stores * exe_env) =
	match ast with
	| InactiveActuator -> (cfae, ase, ee)
	| ActuatorIntAction(act) -> frst_eval_actuator loc nnode cfae ase ee act
	| ActuatorCommand(commands, act_proc) ->
			let node_cfa = take_node_cfa nnode cfae in
			let cmds_list = from_terms_to_vars commands in
			let new_cfa = 
				{
					abs_cfa_store = node_cfa.abs_cfa_store;
					abs_net_env = node_cfa.abs_net_env;
					abs_data_coll = node_cfa.abs_data_coll;
					abs_actuator_dec = (update_act_commands loc cmds_list node_cfa.abs_actuator_dec);
				} in
			let new_cfae = update_node_cfa nnode new_cfa cfae in
			frst_eval_actuator loc nnode new_cfae ase ee act_proc
	| ACloseIter -> (cfae, ase, ee)
	| AOpenIter(act) -> frst_eval_actuator loc nnode cfae ase ee act


let rec frst_eval_component (nnode : int) (cfae : cfa_env) (ase : abs_iot_stores) (ee : exe_env) (ast : component) : (cfa_env * abs_iot_stores * exe_env) =
	match ast with
	| InactiveComponent -> (cfae, ase, ee)
	| ParallelComponent(compa, compb) ->
			let (new_cfae, new_ase, new_ee) = frst_eval_component nnode cfae ase ee compa in
			frst_eval_component nnode new_cfae new_ase new_ee compb
	| Process(name, proc) ->
		let new_proc_env = (name, proc, Free) in
		(cfae, ase, (add_process_exe_env nnode new_proc_env ee))
	| Sensor(loc, sens) -> (cfae, ase, ee)
	| Actuator(loc, act) -> frst_eval_actuator loc nnode cfae ase ee act 


let rec frst_eval_node_definition (cfa_env : cfa_env) (ase : abs_iot_stores) (ee : exe_env) (ast : node) : (cfa_env * abs_iot_stores * exe_env) =
	match ast with
	| ParallelNodes(nodea, nodeb) ->
			let (new_cfae, new_ase, new_ee) = frst_eval_node_definition cfa_env ase ee nodea in
			frst_eval_node_definition new_cfae new_ase new_ee nodeb
	| Node(nnode, compa) ->
			frst_eval_component nnode cfa_env ase ee compa
	| InactNode -> (cfa_env, ase, ee)

	
let rec eval_declaration (nnode : int) (cfa_environment : cfa_env) (abs_stores_env : abs_iot_stores) (exe_env : exe_env) (ast : declaration) : (cfa_env * abs_iot_stores * exe_env) =
	match ast with
	| ParallelDeclarations(decla, declb) ->
		let (new_cfae, new_ase, new_ee) = eval_declaration nnode cfa_environment abs_stores_env exe_env decla in
			eval_declaration nnode new_cfae new_ase new_ee declb
	| SensorDeclaration(loc) -> (cfa_environment, abs_stores_env, exe_env)
	| ActuatorDeclaration(loc) -> ((add_actuator_cfa loc nnode cfa_environment), abs_stores_env, exe_env)
	| VariableDeclaration(name) ->
			((add_var_cfa_env name nnode cfa_environment), (add_var_ase_store name nnode abs_stores_env), exe_env)


let rec eval_node_declaration (cfa_environment : cfa_env) (abs_stores_env : abs_iot_stores) (exe_env : exe_env) (ast : node_declaration) =
	match ast with
	| ParallelNodesDeclaration(decla, declb) ->
			let (new_cfae, new_ase, new_ee) = eval_node_declaration cfa_environment abs_stores_env exe_env decla in
			eval_node_declaration new_cfae new_ase new_ee declb
	| Store(nnode, decl) ->
			let new_cfae = add_node_cfa_env nnode cfa_environment in
			let new_ase = add_node_abs_iot_store nnode abs_stores_env in 
			let new_ee = add_node_exe_env nnode exe_env in
			eval_declaration nnode new_cfae new_ase new_ee decl


let rec frst_eval_IotStructure (cfa_environment : cfa_env) (abs_stores_env : abs_iot_stores) (exe_env : exe_env) (ast : iot_structure) =
	match ast with
	| IoTStructure(decl, _, def) ->
		let (new_cfae, new_ase, new_ee) = eval_node_declaration cfa_environment abs_stores_env exe_env decl in
		frst_eval_node_definition new_cfae new_ase new_ee def