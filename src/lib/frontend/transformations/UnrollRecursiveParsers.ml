open Batteries
open Syntax
open SyntaxUtils
open Collections

(* Unroll recursive parsers: 
  @rec(2, drop) parser foo... 
  becomes
  parser foo ... { drop; }
  parser foo ... { foo; }
  parser foo ... { foo; }
  placed one after another, which works fine in the rest of the pipeline
*)




let replacer =
  object (self)
    inherit [_] s_map as super

    val mutable new_pre_decls = [] (* new decls to add before current *)

  method! visit_decl env decl = 
    (* check if it is a parser with the recursive pragma *)
    match decl.d, Pragma.find_sprag "rec" decl.dpragmas with
    | DParser (id, params, _), Some (_, [n_str; fcn_name]) ->
      (* @rec(n, fcn_name) on parser [id]: the two args are
           n_str    -- the recursion count, as a string (e.g. "3")
           fcn_name -- base case -- must be id "drop" for now *)
      let n = int_of_string n_str in
      if (not (fcn_name = "drop")) then
        failwith
          (Printf.sprintf
             "@rec annotation on parser %s expects base case to be 'drop', but got '%s'"
             (Id.name id)
             (fcn_name));
      (* the unrolled parsers are no longer recursive, so drop the @rec pragma
         (keeping any other pragmas the original carried) *)
      let strip_rec d =
        { d with
          dpragmas =
            List.filter (fun p -> not (Pragma.exists_sprag "rec" [p])) d.dpragmas }
      in
      (* 1. base case parser: same signature, body is just `drop;`. Goes first. *)
      let base_block = ([], (PDrop, decl.dspan)) in
      let base_decl = { (strip_rec decl) with d = DParser (id, params, base_block) } in
      (* 2. n-1 verbatim copies of the original parser, after the base case *)
      let copies = List.init (max 0 (n - 1)) (fun _ -> strip_rec decl) in
      new_pre_decls <- base_decl :: copies;
      (* 3. original parser, with @rec removed *)
      strip_rec decl
    | DParser (id, _, _), Some (_, args) ->
      (* malformed @rec: expected exactly (int, identifier) *)
      failwith
        (Printf.sprintf
           "@rec on parser %s expects (int, identifier), but got %d args"
           (Id.name id)
           (List.length args))
    | _ -> super#visit_decl env decl
    

  method! visit_decls env ds =
    match ds with
    | [] -> []
    | d :: rest ->
      new_pre_decls <- [];                 
      let d' = self#visit_decl env d in    
      let pre = new_pre_decls in           
      new_pre_decls <- [];                 
      let rest' = self#visit_decls env rest in
      pre @ (d' :: rest')

    (* method! visit_DSize env id sz =
      let sz = Option.get sz in
      let sz = self#visit_size env sz in
      env := CidMap.add (Id id) sz !env;
      (* We will filter this declaration later *)
      DSize (id, Some sz)

    method! visit_IUser env cid =
      match CidMap.find_opt cid !env with
      | Some sz -> sz
      | None -> IUser cid *)
  end
;;

let apply ds = replacer#visit_decls () ds
;;
