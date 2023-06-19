(* add an ingress parser to programs with a single packet event, 
   which can be treated as a "default". *)
open CoreSyntax
open MiscUtils

let block actions step : parser_block = 
  List.map (fun a -> a, Span.default) actions, (step, Span.default)
;;

let read cid ty = PRead(cid, ty)
let read_id (id, ty) = read (Cid.id id) ty
let peek cid ty = PPeek(cid, ty)
let skip ty = PSkip(ty)
let assign cid exp = PAssign(cid, exp)

let pbranch ints block : parser_branch  = (List.map (fun i -> PNum (Z.of_int i)) ints), block
let pmatch exps branches = PMatch(exps, branches)
let pgen exp = PGen(exp)
let pdrop = PDrop
let pcall exp = PCall(exp)

let parser id params block = 
  DParser(id, params, block)
;;

let empty_block () :parser_block = 
  block [] pdrop
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