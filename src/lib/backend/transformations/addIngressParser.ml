(* add an ingress parser to programs with a single packet event, 
   which can be treated as a "default". *)
open CoreSyntax





let find_one_in_list fcn lst =
   let rec loop acc = function
      (* at the end of lst, so check if we have found something *)
     | [] -> (match acc with
              | [] -> None
              | [x] -> Some x
              | _ -> failwith "Multiple matches found")
     (* processing the list, so update the accumulator *)
     | x::xs -> if fcn x then loop (x::acc) xs else loop acc xs
   in loop [] lst
;;

let default_event_opt = find_one_in_list 
   (fun decl -> match decl.d with 
      | DEvent(_,_,EPacket, _) -> true
      | _ -> false)
;;

(* sketch of the generated parser 
   for a program with a single default event: 
   <<left off here>> figuring out the right semantics 
   at this point in the IR, and how to transform.
   We might want to add a tagged / untagged event type?
   parser main():
      read intrinsic_md: intrinsic_md_t;
      match (intrinsic_md.ingress_port) with 
      | SOME_INTERNAL_PORT -> {
      }
      | _ -> {
         // default event -- untagged
         read e : untagged_event;
         generate(e);
      }
      | .. -> {
         // read tag : int;
         // problem: "tag" is not defined until TofinoCore.
         // okay so maybe we don't read tag. 
         // we just read "input_event", 
         // then in tofinocore we translate that 
         // into read tag, etc, 
         read e : tagged_event;
         generate(e); 
      }

*)



let add_parser ds = ds