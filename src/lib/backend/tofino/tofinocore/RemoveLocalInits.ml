(* 
   Identify initialization statements in handlers 
   for locals that are _always_ set before they are 
   first read. Remove these initialization statements
   and place the variables into the pre-declared variables list 
   for the handler. *)

open Hashtbl
open CoreSyntax
open TofinoCore

(* this pass centers around assigning each variable a 
   status depending on what happens after it is declared. *)
type var_status = 
  | Declared (* variable is just declared *)
  | Assigned (* variable is declared and the first use of it is _always_ an assignment *)
  | Read     (* variable is declared and in at least 1 ctl flow, the first use of it is a read *)

let status_to_string = function 
  | Declared -> "Declared"
  | Assigned -> "Assigned"
  | Read -> "Read"
;;

(* the trickiest part of variable statuses is merging the 
symbol tables of sibling branches.  *)

(* fold the status of two sibling branches *)
let fold_status s1 s2 = 
  match (s1, s2) with  
  | Some(Assigned), Some(Assigned) -> Some(Assigned)
  (* if the variable is read in either branch, then its read in the join *)
  | Some(Read) ,  _
  | _ , Some(Read) -> Some(Read)
  | Some(Declared), Some(Assigned)
  | Some(Assigned), Some(Declared) -> Some(Declared)
  | Some(Declared), Some(Declared) -> Some(Declared)
  | None, _ -> s2
  | _, None -> s1
;;

(* merge variable status after a branch, for variables declared 
   in scope before the branch. *)
let merge_status s_pre s_branches =  
  (*  I _think_ declared is the right starting point for the fold? *)
  let s_branches = List.map (fun s -> Some(s)) s_branches in
  let s_all_branches = List.fold_left fold_status None s_branches in
  match (s_pre, s_all_branches) with 
  | Declared, Some(Assigned) -> Assigned
  | Declared, Some(Read) -> Read
  | Declared, Some(Declared) -> Declared
  | Declared, None -> Declared
  | Assigned, _ -> Assigned
  | Read, _ -> Read
;;

(* merge symbol tables after a branch *)
let merge_symbols 
    (symbols_pre : (cid * var_status) list) 
    (branch_symbols : ((cid * var_status) list) list)
    : (cid * var_status) list = 
  (* for each symbol in symbol_pre, return merged symbols1 and symbols2 status
      all other variables should appear in one or the other *)
  let cids_pre = List.split symbols_pre |> fst in
  let symbols_post = List.map 
    (fun cid -> 
      (* print_endline ("merging cid: "^(Cid.to_string cid)); *)
      (cid, merge_status 
        (List.assoc cid symbols_pre)  
        (List.map (fun symbols -> List.assoc cid symbols) branch_symbols)
        ))
    cids_pre
  in
  (* for each symbol that was _not_ in symbols_pre, just add their status 
    (okay because varids are unique) *)
  let symbols_post = List.fold_left
    (fun symbols_post symbols_branch -> 
      List.fold_left 
        (fun symbols_post (cid, status) -> 
          match List.assoc_opt cid symbols_post with
          | None -> (cid, status)::symbols_post
          | Some _ -> symbols_post)
        symbols_post
        symbols_branch)
    symbols_post
    branch_symbols
  in
  symbols_post
;;

let ht_to_list ht =
  Hashtbl.to_seq ht |> List.of_seq
;;


let vars_in_exp exp : cid list = 
  let vars_read = object (self)
    inherit [_] s_iter as super
    method vars = Hashtbl.create 128;
    method! visit_EVar _ cid =
      Hashtbl.replace (self#vars) cid true
    end
  in
  vars_read#visit_exp () exp;
  Hashtbl.to_seq_keys vars_read#vars |> List.of_seq
;;

let update_symbols symbols exp = 
  List.iter
    (fun cid -> 
      match (find_opt symbols cid) with
      | Some(Declared) -> 
        (* a variable is used after it is declared, 
           without an intermediate assignment. It is not 
           removable. *)
          Hashtbl.replace symbols cid Read
      | _ -> ()
        (* used without a declaration. 
           This can happen, e.g., params. 
           Not our job to check. *)
    )
  (vars_in_exp exp)
;;

(* find the variables that can be preallocated *)
let find_vars_to_prealloc stmt = 
  let symbols = create 128 in

  let traverser = object (_)
    inherit [_] s_iter as super
      method! visit_exp symbols exp = 
        (* update symbol table for the expression *)
        update_symbols symbols exp;
  
      method! visit_SLocal symbols id ty exp =
        (* post-visit locals (so we hit exp first) *)
        super#visit_SLocal symbols id ty exp;
        (* now, update the symbols table for the newly declared var *)
        Hashtbl.replace symbols (Cid.id id) Declared;

      method! visit_SAssign symbols cid exp =
        super#visit_SAssign symbols cid exp;
        match (find_opt symbols (cid)) with
        | Some(Declared) -> 
          (* was declared, now is declared and assigned *)
          Hashtbl.replace symbols (cid) Assigned;
        | _ -> ()

      (* we need to handle branches, ensuring to update the symbols table at the end *)
      method! visit_SIf symbols exp stmt1 stmt2 =
        (* first visit the expression to update symbols  *)
        super#visit_exp symbols exp;
        (* we give each statement a copy of the symbols table. *)
        let symbols1 = copy symbols in
        let symbols2 = copy symbols in
        (* visit each statement.... which will mutate their symbol tables *)
        super#visit_statement symbols1 stmt1;
        super#visit_statement symbols2 stmt2;
        (* now merge the updated symbols table back into our own *)
        let ss1 = ht_to_list symbols1 in
        let ss2 = ht_to_list symbols2 in
        let ss = ht_to_list symbols in
        let ss = merge_symbols ss [ss1; ss2] in
        Hashtbl.replace_seq symbols (List.to_seq ss)
      method! visit_SMatch symbols exps branches =
        List.iter (super#visit_exp symbols) exps;
        let bstmts = List.split branches |> snd in
        let bstmt_ss = List.map
          (fun bstmt -> 
            let sym = copy symbols in
            (* visit it *)
            super#visit_statement sym bstmt;
            let ss = ht_to_list sym in 
            ss)            
          bstmts
        in
        let ss = ht_to_list symbols in
        let ss = merge_symbols ss bstmt_ss in
        Hashtbl.replace_seq symbols (List.to_seq ss)
    end
  in
  (* traverse the program to build up the symbols table *)
  traverser#visit_statement symbols stmt;    
  (* now we look in the symbols table for all the cids with status "Assigned"  *)
  let assigned_cids = List.filter_map
    (fun (cid, status) -> 
      (* if (Cid.to_string cid ) *)
      (* (print_endline ((Cid.to_string cid)^": "^(status_to_string status))); *)
      match status with
      | Assigned -> Some(cid)
      | _ -> None)
    (to_seq symbols |> List.of_seq)
  in  
  assigned_cids 
;;

(* delete the declarations of all variables in given list. 
   return types of all variables.  *)
let eliminate_vars cids stmt = 
  let eliminated_vartys = ref [] in
  let eliminator = object (_)
    inherit [_] s_map as super
    method! visit_SLocal cids id ty exp = 
      (* if this is a declaration of a cid to be deleted... *)
      if (List.exists 
          (fun cid -> Cid.equal (Cid.id id) cid) 
          cids)
      then (
        (* update eliminated vartys and delete stmt *)
        (* print_endline ("[eliminate_vars] deleting declaration for: "^(CorePrinting.id_to_string id)); *)
        eliminated_vartys := (id, ty)::(!eliminated_vartys);
        SNoop
      )
      else(
        (* just return unchanged *)
        super#visit_SLocal cids id ty exp)
    end
  in

  let stmt = eliminator#visit_statement cids stmt in
  stmt, (!eliminated_vartys)
;;


let process (core_prog : prog) = 
  Base.List.map core_prog
    ~f:(fun component -> 
      let comp_decls' = Base.List.map component.comp_decls
        ~f:(fun decl -> match decl.td with
            | TDHandler(HEvent(h)) -> (
              (* we're collecting the new declared variables in this handler *)
              let new_declared_locals = ref [] in
              let hdl_body' = 
                match h.hdl_body with
                | SFlat(stmt) -> 
                  let prealloc_cids  = find_vars_to_prealloc stmt in
                  let stmt', locals = eliminate_vars prealloc_cids stmt in
                  new_declared_locals := (!new_declared_locals) @ locals;
                  SFlat(stmt')
                | SPipeline(_) ->
                  error "[RemoveLocalInits] this pass should be run before pipelining"
              in
              let h' = HEvent({h with 
                hdl_body = hdl_body'; 
                hdl_preallocated_vars = add_preallocated_locals h.hdl_preallocated_vars (!new_declared_locals)}) 
              in
              let td' = TDHandler(h') in
              { decl with td = td'; }
            )
            (* non-handler declaration -- do nothing *)
            | _ -> decl
        )
    in
    let component' = { component with comp_decls = comp_decls';} in 
    component'
    )
