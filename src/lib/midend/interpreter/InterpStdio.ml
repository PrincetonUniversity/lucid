open Batteries
exception Error of string

let error s = raise (Error s)

let sighdl s =
  print_endline ("got signal " ^ string_of_int s);
  if s != -14 then exit 1
;;

let blocking_read_line fd  =
  let (ready_fds, _, _) = Unix.select [fd] [] [] (-1.0) in
  if List.mem fd ready_fds then
    try Some (input_line ( Unix.input_of_descr ~autoclose:false fd))
    with End_of_file -> None
  else
    error "a blocking select completed without the fd being ready"
;;

let wait_until_ready fd = 
  let (ready_fds, _, _) = Unix.select [fd] [] [] (-1.0) in
  if List.mem fd ready_fds then true
  else false
;;
let check_if_ready fd = 
  let (ready_fds, _, _) = Unix.select [fd] [] [] (0.0) in
  if List.mem fd ready_fds then true
  else false
;;

let ends_with_newline buffer = 
  (Buffer.nth buffer (Buffer.length buffer - 1)) = '\n'
;;
(* read a block of bytes, extending it as necessary to reach a newline *)
let read_lines fd = 
  let buffer = Buffer.create 64 in
  let block = Bytes.create 64 in
  let n = Unix.read fd block 0 64 in
  Buffer.add_subbytes buffer block 0 n;
  (* eof -- return nothing read *)
  if (n = 0) then []
  else 
    if (ends_with_newline buffer) then (
      (* split on \n and return *)
      Str.split (Str.regexp "\n") (Buffer.contents buffer)
    )
    else (
      (* read until there's a newline or eof *)
      let newline = ref false in
      let eof = ref false in
      while ((not (!newline)) && (not (!eof))) do
        let n = Unix.read fd block 0 1 in
        if (n = 0) then (eof := true)
        else (      
          Buffer.add_subbytes buffer block 0 n;
          if (ends_with_newline buffer) then (
            newline := true;
          )
        )
      done;
      (* eof returns nothing read *)
      if (!eof) then []
      else ( Str.split (Str.regexp "\n") (Buffer.contents buffer))
    )  

let blocking_read_lines fd = read_lines fd ;;

(* assume no output from non blocking means not ready *)
let non_blocking_read_lines fd =  
  if (check_if_ready fd) 
    then read_lines fd
    else []
;;

type interactive_mode_input =
  | Events of InterpJson.interp_input list
  | NoEvents (* no new events, but the file is not closed *)
  | End (* the file is closed *)
;;

let get_stdio_input block pp renaming num_switches current_time =
  let parse_input_str ev_str = InterpSpec.parse_interp_event_list
    pp
    renaming
    num_switches
    current_time
    (Yojson.Basic.from_string ev_str)
  in
  if (block) then (
    let ev_strs = blocking_read_lines Unix.stdin in
    match ev_strs with
    | [] -> End (* eof is the only option here *)
    | ev_strs -> 
      Events (List.map parse_input_str ev_strs |> List.flatten)
  )
  else (
    let ev_strs = non_blocking_read_lines Unix.stdin in
    match ev_strs with
      | [] -> NoEvents (* ignore eof until a blocking read *)
      | ev_strs -> 
        Events (List.map parse_input_str ev_strs |> List.flatten)
  )
;;
