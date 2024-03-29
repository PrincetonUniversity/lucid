(* check foriegn function defintions by piping the 
   function signature and definition to gcc *)

open CCoreSyntax


(* gcc syntax checker *)

let check_ffun (f: ffun) : bool =
  let open CCorePPrint in 
  let open Caml in
  let open Caml_unix in
  let input_all ic =
    let rec loop acc =
      try
        let line = input_line ic in
        loop (acc ^ line ^ "\n")
      with End_of_file ->
        acc
    in
    loop ""
  in

  match f.check_cmd with
  | None -> true  (* If there's no check lscommand, assume the function is correct *)
  | Some cmd -> (
    (* Generate the function signature *)
    let signature = ty_to_string f.fret_ty ^ " "^ cid_to_string f.fid ^"(" ^params_to_string f.fparams ^ ");"  in

    (* Combine the signature and the function definition *)
    let full_function = signature ^ "\n" ^ f.fstr in

    (* Run the check command with the function as input *)
    let ic, oc, ec = Caml_unix.open_process_full cmd (Caml_unix.environment ()) in
    output_string oc full_function;
    close_out oc;

    (* Read the output and error messages *)
    let output = input_all ic in
    let errors = input_all ec in

    (* Close the process and get the exit status *)
    let status = Caml_unix.close_process_full (ic, oc, ec) in

    (* If the check command succeeded, return true. Otherwise, print the errors and return false. *)
    match status with
    | Unix.WEXITED 0 -> true
    | _ -> 
      print_endline ("---- type checking of foriegn function failed ----");
      print_endline ("input: ");
      print_endline full_function;
      print_endline ("command: "^(cmd));
      print_endline ("---- errors ----");
      print_endline errors; 
      print_endline ("---- stdout ----");
      print_endline output;
      false

    (* Run the check command on the file *)
    (* let check_result = Sys.command (cmd ^ " " ^ filename) in *)
  )

  ;;



let check fds : unit =
  let v = object 
    inherit [_] s_iter as super
    method! visit_ffun () ffun = 
      let _ = check_ffun ffun in ()
    end
  in
  v#visit_decls () fds     
;;


(* example usage *)
(* let test_prog = 
  [
    decl (DFFun{
      fid=Id.create "add";
      fparams = [(Id.create "x", tint 32); Id.create "y", tint 32];
      fret_ty = tint 32;
      fstr = 
        "int add(int x, int y) {return x + y;}"
      ;
      check_cmd = default_checker;
    }) Span.default
  ]
;;

check test_prog;;
exit 1; *)