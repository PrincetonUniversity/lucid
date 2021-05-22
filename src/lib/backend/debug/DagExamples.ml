
(***************** testing helpers ******************)
open InstrSyntax
open MiscUtils
open DagSyntax
open P4IdPrint

[@@@ocaml.warning "-17-8-27"]


(* return decls map for a simple increment action *)
let gen_incr_acn acn_id incr_var_id next_tids = 
	let ivec_id = Cid.fresh [("incr_"^mid_to_str incr_var_id)] in
	let ivec = InstrVec(ivec_id, [incr_var_id, (BinOp(Add, Meta(incr_var_id), Const(Integer.of_int 1)))]) in 
	let acn = Action(acn_id, [ivec_id], next_tids) in 
	[(ivec_id, ivec); (acn_id, acn)]
;;

(* build an action that sets a variable to a constant value *)
let gen_set_acn acn_id set_var_id set_var_val next_tids = 
	let ivec_id = Cid.fresh [("set_"^mid_to_str set_var_id)] in
	let ivec = InstrVec(ivec_id, [set_var_id, Oper(Const(Integer.of_int set_var_val))]) in 
	let acn = Action(acn_id, [ivec_id], next_tids) in 
	[(ivec_id, ivec); (acn_id, acn)]
;;

(* build an action that sets a variable the sum of a variable + constant *)
let gen_eval_plus acn_id set_var_id op_1_var_id const_val next_tids = 
	let ivec_id = Cid.fresh [("add_"^mid_to_str set_var_id)] in
	let ivec = InstrVec(ivec_id, [set_var_id, (BinOp(Add, Meta(op_1_var_id), Const(Integer.of_int const_val)))]) in 
	let acn = Action(acn_id, [ivec_id], next_tids) in 
	[(ivec_id, ivec); (acn_id, acn)]
;;

let gen_empty_acn acn_id next_tids = 
	let acn = Action(acn_id, [], next_tids) in 
	[(acn_id, acn)]
;;

(* generate decls map for a table that applies acn_id if (cond_var_id == cond_var_val) *)
let gen_cond_tbl tbl_id cond_var_id cond_var_val acn_id = 
	let const_cond = Exact(Integer.of_int cond_var_val) in 
	let pat = [(cond_var_id, const_cond)] in 
	let rule = Match(Cid.fresh ["r"], pat, acn_id) in 
	let tbl = Table(tbl_id, [rule], None) in 
	[(tbl_id, tbl)]
;;

(* generate a table that always applies acn_id *)
let gen_call_tbl tbl_id acn_id = 
	let always_apply_rule = Match(Cid.fresh ["r"], [], acn_id) in 
	let tbl = Table(tbl_id, [always_apply_rule], None) in 
	[(tbl_id, tbl)]
;;

(* generate a table that branches to two different actions based on a condition variable value *)
let gen_branch_tbl tbl_id cond_var_id cond_var_val eq_acn_id neq_acn_id = 
	let const_cond = Exact(Integer.of_int cond_var_val) in 
	let eq_pat = [(cond_var_id, const_cond)] in 
	let eq_rule = Match(Cid.fresh ["r"], eq_pat, eq_acn_id) in 
	let neq_pat = [(cond_var_id, Any)] in 
	let neq_rule = Match(Cid.fresh ["r"], neq_pat, neq_acn_id) in 
	let tbl = Table(tbl_id, [eq_rule; neq_rule], None) in 
	[(tbl_id, tbl)]
;;

let gen_simple_table_ex () = 
	(* 
		two tables to implement two statements: 
		x++;
		y++;
		increment vars: x, y
		condition vars: s_v, t_v
		actions: s_a, t_a
		tables: s, t
	 *)
	let [x; y; a; b; sa; ta; s; t] = 
		["x"; "y"; "a"; "b"; "sa"; "ta"; "s"; "t"] |> 
		CL.map Id.create |>
		CL.map Cid.id
	in 
	(* generate decls map with s_a and t_a *)	
	let decls_map = gen_incr_acn sa x [t] in 
	let decls_map = decls_map@(gen_incr_acn ta y []) in 
	(* add s and t *)
	let decls_map = decls_map@(gen_cond_tbl s a 1 sa) in
	let decls_map = decls_map@(gen_cond_tbl t b 2 ta) in
	(s, t, decls_map)
;;

let gen_nested_if_tbl_ex () = 
	(* 
		tables to implement a nested if
		if (x == 1) {
			a++;
			if (y == 2) {
				c++;				
			}
			else {
				d++;
			}
		}
		else {
			b++;
		}
		increment vars: a, b, c, d
		condition vars: x, y
		actions: acn_a, acn_b, acn_c, acn_d
		tables: s, t
	 *)
	let [a; b; c; d; x; y; acn_a; acn_b; acn_c; acn_d; tbl_x; tbl_y] = 
		["a"; "b"; "c"; "d"; "x"; "y"; "acn_a"; "acn_b"; "acn_c"; "acn_d"; "tbl_x"; "tbl_y"] |> 
		CL.map Id.create |>
		CL.map Cid.id
	in 
	(* decls map with actions *)	
	let decls_map = CL.fold_left (fun decls_map (acn_id, incr_var, next_tid) -> decls_map@(gen_incr_acn acn_id incr_var next_tid)) [] [(acn_a, a, [tbl_y]); (acn_b, b, []); (acn_c, c, []); (acn_d, d, [])] in 

	(* tables *)
	let decls_map = decls_map@(gen_branch_tbl tbl_x x 1 acn_a acn_b) in 
	let decls_map = decls_map@(gen_branch_tbl tbl_y y 2 acn_c acn_d) in 

	tbl_x, tbl_y, decls_map
;;

let gen_infeasible_tup_ex () = 
	(* 
		generates two parallel tables with an infeasible pair of actions. 
		table 1: 
			(x == 1) : a++;
			(x == _) : b++;
		table 2:
			(x == 2) : c++;
			(x == _) : d++;
		expected merged table: 
			(x == 1) : a++; d++;
			(x == 2) : b++; c++;
			(x == _) : b++; d++;
		Main point: no path in the merged table should execute a++; c++, 
		as that path would require x == 1 and x == 2 -- not feasible.
	*)
	let [a; b; c; d; x; acn_a; acn_b; acn_c; acn_d; tbl_x1; tbl_x2] = 
		["a"; "b"; "c"; "d"; "x"; "acn_a"; "acn_b"; "acn_c"; "acn_d"; "tbl_x1"; "tbl_x2"] |> 
		CL.map Id.create |>
		CL.map Cid.id
	in 
	(* actions with incrementers for a, b, c, d *)	
	let decls_map = CL.fold_left (fun decls_map (acn_id, incr_var, next_tid) -> decls_map@(gen_incr_acn acn_id incr_var next_tid)) [] [(acn_a, a, []); (acn_b, b, []); (acn_c, c, []); (acn_d, d, [])] in 

	(* tables x1 and x2 *)
	let decls_map = decls_map@(gen_branch_tbl tbl_x1 x 1 acn_a acn_b) in 
	let decls_map = decls_map@(gen_branch_tbl tbl_x2 x 2 acn_c acn_d) in 
	tbl_x1, tbl_x2, decls_map
;;

let gen_multitable_branch_ex () = 
	(* 
		a program with a branch that performs data dependent operations
		that must be executed in separate tables.
		a = 1;
		if (x == 1){
			b = a + 1;	
		}
		else {
			b = a + 2;
		}
		c = b + 1;
	*)
	let [a; b; c; x; acn_a; acn_branchx_1; acn_branchx_2; acn_b1; acn_b2; acn_c; tbl_seta; tbl_branchx; tbl_setb1; tbl_setb2; tbl_setc] = 
		["a"; "b"; "c"; "x"; "acn_a"; "acn_branchx_1"; "acn_branchx_2"; "acn_b1"; "acn_b2"; "acn_c"; "tbl_seta"; "tbl_branchx"; "tbl_setb1"; "tbl_setb2"; "tbl_setc"] |>
		CL.map Id.create |>
		CL.map Cid.id
	in 
	let decls_map = []
		(* a = 1; *)
		@(gen_call_tbl tbl_seta acn_a)
		@(gen_set_acn acn_a a 1 [tbl_branchx])
		(* if (x == 1){
		} 
		else {
		} *)		
		@(gen_branch_tbl tbl_branchx x 1 acn_branchx_1 acn_branchx_2)
		@(gen_empty_acn acn_branchx_1 [tbl_setb1])
		@(gen_empty_acn acn_branchx_2 [tbl_setb2])
		(* b = a + 1; *)
		@(gen_call_tbl tbl_setb1 acn_b1)
		@(gen_eval_plus acn_b1 b a 1 [tbl_setc])
		(* b = a + 2; *)
		@(gen_call_tbl tbl_setb2 acn_b2)
		@(gen_eval_plus acn_b2 b a 2 [tbl_setc])
		(* c = b + 1; *)
		@(gen_call_tbl tbl_setc acn_c)
		@(gen_eval_plus acn_c c b 1 [])
	in
	tbl_seta, tbl_branchx, decls_map
;;

let gen_multitable_partial_data_dep_ex () = 
	(* 
		a program with a branch that performs data dependent operations
		that must be executed in separate tables.
		a = 1;
		if (x == 1){
			b = a + 1;	
		}
		else {
			b = 2;
		}
		c = b + 1;
	*)
	let [a; b; c; x; acn_a; acn_branchx_1; acn_branchx_2; acn_b1; acn_b2; acn_c; tbl_seta; tbl_branchx; tbl_setb1; tbl_setb2; tbl_setc] = 
		["a"; "b"; "c"; "x"; "acn_a"; "acn_branchx_1"; "acn_branchx_2"; "acn_b1"; "acn_b2"; "acn_c"; "tbl_seta"; "tbl_branchx"; "tbl_setb1"; "tbl_setb2"; "tbl_setc"] |>
		CL.map Id.create |>
		CL.map Cid.id
	in 
	let decls_map = []
		(* a = 1; *)
		@(gen_call_tbl tbl_seta acn_a)
		@(gen_set_acn acn_a a 1 [tbl_branchx])
		(* if (x == 1){
		} 
		else {
		} *)		
		@(gen_branch_tbl tbl_branchx x 1 acn_branchx_1 acn_branchx_2)
		@(gen_empty_acn acn_branchx_1 [tbl_setb1])
		@(gen_empty_acn acn_branchx_2 [tbl_setb2])
		(* b = a + 1; *)
		@(gen_call_tbl tbl_setb1 acn_b1)
		@(gen_eval_plus acn_b1 b a 1 [tbl_setc])
		(* b = 2; *)
		@(gen_call_tbl tbl_setb2 acn_b2)
		@(gen_set_acn acn_b2 b 2 [tbl_setc])
		(* c = b + 1; *)
		@(gen_call_tbl tbl_setc acn_c)
		@(gen_eval_plus acn_c c b 1 [])
	in
	tbl_seta, tbl_branchx, decls_map

;;


let gen_stateful_simple () = 
	let [a_reg; b_reg; a; b; 
	x; tbl_branch_x; acn_branch_x_0; acn_branch_x_1; 
	tbl_set_a_0; acn_set_a_0; tbl_set_a_1; acn_set_a_1; tbl_set_b; acn_set_b] = 
		["a_reg"; "b_reg";"a";"b";
		"x";"tbl_branch_x"; "acn_branch_x_0"; "acn_branch_x_1"; 
		"tbl_set_a_0"; "acn_set_a_0"; "tbl_set_a_1"; "acn_set_a_1"; "tbl_set_b"; "acn_set_b"]
		|> CL.map Id.create |> CL.map Cid.id
	in

	1