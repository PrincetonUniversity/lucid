(* Mutable program state within a single switch in the interpreter. *)
open Batteries
open CoreSyntax

(*** pipeline objects ***)
type obj =
  | OArray of arrobj
  (* "None" cases are un-used entries *)
  | OTable of tblobj

and arrobj =
  { aid : id
  ; acells : zint array
  ; ispair : bool
  }

and tblobj =
  { tid : id
  ; (* sentries: tbl_entry list; *)
    tmaxlen : int
  ; tentries : tbl_entry list
  ; tdefault : id * exp list (* default action + value args *)
  }

(*** the pipeline ***)
type t =
  { objs : obj array
  ; current_stage : int ref
  }

(*** object operations ***)
let copy_obj s =
  match s with
  | OArray a -> OArray { a with acells = Array.copy a.acells }
  (* there is nothing mutable in a table *)
  | OTable t -> OTable { t with tentries = t.tentries }
;;

let obj_to_id o =
  match o with
  | OArray { aid; _ } -> aid
  | OTable { tid; _ } -> tid
;;

let obj_to_name o = Id.name (obj_to_id o)

let split_integer n =
  let sz = Integer.size n / 2 in
  let upper = Integer.shift_right n sz |> Integer.set_size sz in
  let lower = Integer.set_size sz n in
  upper, lower
;;

let stage_to_string ?(pad = "") show_ids idx s =
  match s with
  | OArray a ->
    let print_entry n =
      if not a.ispair
      then Integer.to_string n
      else (
        let upper, lower = split_integer n in
        Printf.sprintf
          "(%s, %s)"
          (Integer.to_string upper)
          (Integer.to_string lower))
    in
    if show_ids
    then (
      (* don't print long arrays (use control commands to inspect them) *)
      let contents_str =
        if Array.length a.acells > 1024
        then "[...]"
        else Printing.list_to_string print_entry (Array.to_list a.acells)
      in
      Printf.sprintf
        "%s%s(%d) : %s\n"
        (pad ^ pad)
        (CorePrinting.id_to_string a.aid)
        idx
        contents_str)
    else
      (* old printer, just for testing *)
      Printf.sprintf
        "%s%d : %s\n"
        (pad ^ pad)
        idx
        (Printing.list_to_string print_entry (Array.to_list a.acells))
  | OTable t ->
    Printf.sprintf
      "%s%s(%d) : Table [...]\n"
      (pad ^ pad)
      (CorePrinting.id_to_string t.tid)
      idx
;;

(*** pipeline operations ***)
let empty () =
  { objs =
      Array.make
        0
        (OArray
           { aid = Id.create ""
           ; acells = Array.make 0 (Integer.of_int 0)
           ; ispair = false
           })
      (* (Array.make 0 (Integer.of_int 0), false) *)
  ; current_stage = ref 0
  }
;;

let append t obj = { t with objs = Array.append t.objs @@ Array.make 1 obj }
let reset_stage t = t.current_stage := 0

let copy t =
  (* { arrs = Array.map (fun (arr, o) -> Array.copy arr, o) t.arrs *)
  { objs = Array.map copy_obj t.objs; current_stage = ref !(t.current_stage) }
;;

let length t = Array.length t.objs

let to_string ?(pad = "") t =
  if Array.length t.objs = 0
  then "[ ]"
  else (
    let str =
      t.objs
      |> Array.mapi (stage_to_string ~pad true)
      |> Array.fold_left ( ^ ) ""
    in
    Printf.sprintf "[\n%s%s]" str pad)
;;

let get_obj_unconstrained stage t =
  if stage < 0 then failwith "Pipeline Error: Stage is negative";
  if stage >= Array.length t.objs
  then
    failwith
      "Pipeline Error: Attempted to access nonexistent global. This should be \
       impossible.";
  t.objs.(stage)
;;

let get_obj stage t =
  (* load the object from the pipe *)
  if stage < 0 then failwith "Pipeline Error: Stage is negative";  
  (* we may want this to double check that accesses are ordered. 
     (I don't think it matters though) *)
  (* if (not (Cmdline.cfg.unordered) && stage < !(t.current_stage))
  then
    failwith
      "Pipeline Error: Attempted to access global out-of-order. The type \
       system should have caught this, so notify the developer."; *)
  if stage >= Array.length t.objs
  then
    failwith
      "Pipeline Error: Attempted to access nonexistent global. This should be \
       impossible.";
  t.objs.(stage)
;;

let set_obj stage t obj = t.objs.(stage) <- obj

(* find object stage by id, for control operations *)
let id_map t =
  List.mapi (fun idx obj -> obj_to_id obj, idx) (Array.to_list t.objs)
;;

let id_to_stage id t =
  match List.assoc_opt id (id_map t) with
  | None ->
    error
      ("[pipeline error] could not find global object "
      ^ CorePrinting.id_to_string id)
  | Some stage_num -> stage_num
;;

(*** array functions ***)
let mk_array ~(id : Id.t) ~(width : int) ~(length : int) ~(pair : bool) =
  let full_width = if pair then width * 2 else width in
  let new_cells = Array.make length (Integer.create 0 full_width) in
  OArray { aid = id; acells = new_cells; ispair = pair }
;;

let validate_arr_idx idx obj =
  (* is an update to obj[idx] valid? *)
  match obj with
  | OArray a ->
    if idx < 0
    then failwith @@ Printf.sprintf "Pipeline Error: Index %d is negative." idx;
    if idx >= Array.length a.acells
    then
      failwith
      @@ Printf.sprintf
           "Pipeline Error: Index %d is invalid for global object %s."
           idx
           (CorePrinting.id_to_string a.aid);
    obj
  | OTable _ ->
    failwith @@ Printf.sprintf "Pipeline Error: table updates not implemented"
;;

let update
  ~(stage : int)
  ~(idx : int)
  ~(getop : zint -> 'a)
  ~(setop : zint -> zint)
  (t : t)
  =
  let obj = get_obj stage t |> validate_arr_idx idx in
  match obj with
  | OArray a ->
    if a.ispair
    then failwith "Pipeline Error: Tried to use Array.update on a paired array";
    let orig_val = a.acells.(idx) in
    a.acells.(idx) <- setop orig_val;
    t.current_stage := stage + 1;
    getop orig_val
  | OTable _ -> failwith "Pipeline Error: Tried to use Array.update on a table"
;;

let update_complex
  ~(stage : int)
  ~(idx : int)
  ~(memop : zint -> zint -> zint * zint * 'a)
  (t : t)
  =
  let obj = get_obj stage t |> validate_arr_idx idx in
  match obj with
  | OArray a ->
    let orig_val = a.acells.(idx) in
    let new_val, ret =
      match a.ispair with
      | true ->
        let upper, lower = split_integer orig_val in
        let new_upper, new_lower, ret = memop upper lower in
        Integer.concat new_upper new_lower, ret
      | false ->
        let new_val, _, ret =
          memop orig_val (Integer.create ~value:0 ~size:(Integer.size orig_val))
        in
        new_val, ret
    in
    a.acells.(idx) <- new_val;
    t.current_stage := stage + 1;
    ret
  | OTable _ ->
    failwith @@ "Pipeline Error: Tried to use Array.update_complex on a table"
;;

(*** table functions ***)

(* allocate a table with room for n entries *)
let mk_table ~(id : Id.t) ~(length : int) ~(def : id * exp list) =
  (* wrap in pipeline object *)
  OTable
    { tid = id; tdefault = def; tmaxlen = length; tentries = [] }
;;

let get_table_entries
  ~(* read the entries in a table for a table match *)
  (stage : int)
  (t : t)
  : (id * exp list) * tbl_entry list
  =
  let obj = get_obj stage t in
  match obj with
  | OTable tbl ->
    (* advance stage and return default + list of cases *)
    t.current_stage := stage + 1;
    (* print_endline ("[get_table_entries] number of entries: "^(string_of_int (List.length tbl.sentries))); *)
    tbl.tdefault, tbl.tentries
  | OArray _ -> failwith "Pipeline Error: Expected a table obj, got array obj."
;;

let install_table_entry ~(stage : int) ~(entry : tbl_entry) (t : t) =
  (* get the table object *)
  let obj = get_obj_unconstrained stage t in
  match obj with
  | OTable tbl ->
    (* install semantics:
        - entries are always sorted by priority (lowest priority first)
        - a new entry is added immediately before the first entry with a higher priority *)
    (* print_endline ("[install_table_entry] installing table entry.. "); *)
    (* CorePrinting.entry_to_string entry |> print_endline; *)
    (* print_endline ("[install_table_entry] ----"); *)
    if List.length tbl.tentries == tbl.tmaxlen
    then failwith "Pipeline Error: tried to add an entry to a full table!";
    let entries =
      match tbl.tentries with
      | [] -> [entry]
      | _ ->
        let added, entries_rev =
          List.fold_left
            (fun (added, entries_rev) cur_entry ->
              if added (* nothing else to do *)
              then added, cur_entry :: entries_rev
              else if (* reached an entry with higher prio, put new entry before it (after in reversed list) *)
                      cur_entry.eprio > entry.eprio
              then true, cur_entry :: entry :: entries_rev
              else false, cur_entry :: entries_rev)
            (false, [])
            tbl.tentries
        in
        let entries =
          if added then List.rev entries_rev else List.rev (entry :: entries_rev)
        in
        entries
    in
    (* update stage with new table *)
    set_obj stage t (OTable { tbl with tentries = entries })
  | _ -> error "Pipeline Error: object is not a table"
;;

(*** control command functions.
     note: generally, these reference objects by name rather than stage ***)

let control_set ~(id : Id.t) ~(idx : int) ~(newvals : zint list) (t : t) =
  let stage = id_to_stage id t in
  let obj = get_obj_unconstrained stage t |> validate_arr_idx idx in
  match obj with
  | OArray a ->
    if a.ispair
    then (
      match newvals with
      | [upper; lower] ->
        let newval = Integer.concat upper lower in
        a.acells.(idx) <- newval
      | _ -> error "[control_set] wrong number of values for pair array")
    else (
      match newvals with
      | [newval] -> a.acells.(idx) <- newval
      | _ ->
        error "[control_set] gave wrong number of values for non-pair array")
  | _ -> error "[control_set] tried to use on a non-array object"
;;

let control_setrange
  ~(id : Id.t)
  ~(s : int)
  ~(e : int)
  ~(newvals : zint list)
  (t : t)
  =
  let stage = id_to_stage id t in
  let obj =
    get_obj_unconstrained stage t
    |> validate_arr_idx s
    |> validate_arr_idx (e - 1)
  in
  match obj with
  | OArray a ->
    let newval =
      if a.ispair
      then (
        match newvals with
        | [upper; lower] -> Integer.concat upper lower
        | _ -> error "[control_setrange] wrong number of values for pair array")
      else (
        match newvals with
        | [newval] -> newval
        | _ ->
          error
            "[control_setrange] gave wrong number of values for non-pair array")
    in
    for idx = s to e - 1 do
      a.acells.(idx) <- newval
    done
  | _ -> error "[control_setrange] tried to use on a non-array object"
;;

let control_get ~(id : Id.t) ~(idx : int) (t : t) =
  let stage = id_to_stage id t in
  let obj = get_obj_unconstrained stage t |> validate_arr_idx idx in
  match obj with
  | OArray a ->
    if a.ispair
    then (
      let upper, lower = split_integer a.acells.(idx) in
      [upper; lower])
    else [a.acells.(idx)]
  | _ -> error "[control_get] tried to use control_get on a non-array object"
;;

let control_getrange ~(id : Id.t) ~(s : int) ~(e : int) (t : t) =
  let stage = id_to_stage id t in
  let obj =
    get_obj_unconstrained stage t
    |> validate_arr_idx s
    |> validate_arr_idx (e - 1)
  in
  match obj with
  | OArray a ->
    let get_idx ispair idx =
      if ispair
      then (
        let upper, lower = split_integer a.acells.(idx) in
        [upper; lower])
      else [a.acells.(idx)]
    in
    List.map (get_idx a.ispair) (MiscUtils.range s e)
  | _ -> error "[control_get] tried to use control_get on a non-array object"
;;

let control_install_table_entry ~(id : Id.t) ~(entry : tbl_entry) (t : t) =
  let stage = id_to_stage id t in
  install_table_entry stage entry t
;;
