open Batteries
open CoreSyntax


(*** pipeline objects ***)
type obj = 
  | OArray of {sid : id; sarr : zint array; spair : bool}
  (* "None" cases are un-used entries *)
  | OTable  of {sid : id; sactions : action list; scases : (case option) array;}
  | ONone


let mk_array ~(id : Id.t) ~(width : int) ~(length : int) ~(pair : bool) = 
  let full_width = if pair then width * 2 else width in
  let new_arr = Array.make length (Integer.create 0 full_width) in
  OArray({sid=id; sarr=new_arr; spair=pair})  
;;


(* allocate a table with room for n entries *)
let mk_table ~(id : Id.t) ~(length : int) =
  (* let empty_entry = ([], "", []) in  *)
  OTable({sid=id; sactions=[]; scases = Array.make length None;})
;;


let copy_stage s = match s with 
  | OArray(a) -> OArray({a with sarr=Array.copy a.sarr})
  | OTable(t) -> OTable({t with scases=Array.copy t.scases})
  | ONone -> ONone
;;

let split_integer n =
  let sz = Integer.size n / 2 in
  let upper = Integer.shift_right n sz |> Integer.set_size sz in
  let lower = Integer.set_size sz n in
  upper, lower
;;

let stage_to_string ?(pad = "") show_ids idx s =
  match s with
  | OArray(a) -> 
    let print_entry n =
      if not a.spair
      then Integer.to_string n
      else (
        let upper, lower = split_integer n in
        Printf.sprintf
          "(%s, %s)"
          (Integer.to_string upper)
          (Integer.to_string lower))
    in
    if show_ids
    then 
      Printf.sprintf
        "%s%s(%d) : %s\n"
        (pad ^ pad)
        (CorePrinting.id_to_string a.sid)
        idx
        (Printing.list_to_string print_entry (Array.to_list a.sarr))
    else
      (* old printer, just for testing *)
      Printf.sprintf
         "%s%d : %s\n"
         (pad ^ pad)
         idx
         (Printing.list_to_string print_entry (Array.to_list a.sarr))
  | OTable(t) -> 
    Printf.sprintf 
      "%s%s(%d) : Table [...]\n" 
      (pad ^ pad) 
      (CorePrinting.id_to_string t.sid)
      idx
  | ONone -> ""
;;

let validate_arr_idx idx obj = 
  (* is an update to obj[idx] valid? *)
  match obj with
  | OArray(a) -> 
    if (idx < 0)
    then 
      failwith 
      @@ Printf.sprintf 
          "Pipeline Error: Index %d is negative." 
          idx;
    if (idx >= Array.length a.sarr)
    then
      failwith
      @@ Printf.sprintf
          "Pipeline Error: Index %d is invalid for global object %s."
          idx
          (CorePrinting.id_to_string a.sid);
    obj
  | OTable(_) -> 
    failwith @@ Printf.sprintf "Pipeline Error: table updates not implemented"
  | ONone -> 
    failwith @@ Printf.sprintf "Pipeline Error: Cannot update NoneType"

(*** pipeline operations ***)

type t =
  { arrs : obj array
  ; current_stage : int ref
  }


let empty () =
  { arrs = Array.make 
    0 
    ONone
    (* (Array.make 0 (Integer.of_int 0), false) *)
  ; current_stage = ref 0
  }
;;

let append t obj = 
  {t with arrs = Array.append t.arrs @@ Array.make 1 (obj)}  


let reset_stage t = t.current_stage := 0


let copy t =
  (* { arrs = Array.map (fun (arr, o) -> Array.copy arr, o) t.arrs *)
  { arrs = Array.map copy_stage t.arrs
  ; current_stage = ref !(t.current_stage)
  }
;;

let length t = Array.length t.arrs

let to_string ?(pad = "") t =
  if Array.length t.arrs = 0
  then "[ ]"
  else (
    let str =
      t.arrs
      |> Array.mapi (stage_to_string ~pad true)
      |> Array.fold_left ( ^ ) ""
    in
    Printf.sprintf "[\n%s%s]" str pad)
;;

let get_obj stage t = 
  (* load the object from the pipe *)
  if stage < 0 
  then failwith "Pipeline Error: Stage is negative";
  if stage < !(t.current_stage)
  then
    failwith
      "Pipeline Error: Attempted to access global out-of-order. The type \
       system should have caught this, so notify the developer.";
  if stage >= Array.length t.arrs
  then
    failwith
      "Pipeline Error: Attempted to access nonexistent global. This should be \
       impossible.";
  t.arrs.(stage)
;;

let get_table_entries
  (* read the entries in a table for a table match *)
    ~(stage : int)
    (t : t)
  =
  let obj = get_obj stage t in 
  match obj with
  | OTable(tbl) -> 
    (* advance stage and return the list of cases *)
    t.current_stage := stage + 1;
    Array.to_list tbl.scases |> List.filter_map (fun co -> co)
  | OArray(_) -> failwith "Pipeline Error: Expected a table obj, got array obj."
  | ONone -> failwith "Pipeline Error: Expected array, got nonetype."
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
  | OArray(a) ->
    if a.spair
      then failwith "Pipeline Error: Tried to use Array.update on a paired array";
    let orig_val = a.sarr.(idx) in
    a.sarr.(idx) <- setop orig_val;
    t.current_stage := stage + 1;
    getop orig_val
  | OTable(_) -> 
    failwith "Pipeline Error: Tried to use Array.update on a table"
  | ONone -> 
    failwith "Pipeline Error: Tried to use Array.update on none object"
;;

let update_complex
    ~(stage : int)
    ~(idx : int)
    ~(memop : zint -> zint -> zint * zint * 'a)
    (t : t)
  =
  let obj = get_obj stage t |> validate_arr_idx idx in 
  match obj with
  | OArray(a) -> 
    let orig_val = a.sarr.(idx) in
    let new_val, ret = 
      match a.spair with
      | true -> 
        let upper, lower = split_integer orig_val in
        let new_upper, new_lower, ret = memop upper lower in
        Integer.concat new_upper new_lower, ret
      | false -> 
        let new_val, _, ret =
          memop 
            orig_val 
            (Integer.create ~value:0 ~size:(Integer.size orig_val))
        in
        new_val, ret
    in
    a.sarr.(idx) <- new_val;
    t.current_stage := stage + 1;
    ret
  | OTable(_) -> 
    failwith @@ "Pipeline Error: Tried to use Array.update_complex on a table"
  | ONone -> 
    failwith @@ "Pipeline Error: Tried to use Array.update_complex on none object"
;;
