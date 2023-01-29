open Syntax
open PlainRegex
open DFASynthesis
open RegexElimination
open Collections

type env = {prior_events : id list; alph_map : (id list) IdMap.t};;

let make_event_decl id = 
  decl (DEvent (id, EBackground, [], []))

let make_event_decls ids existing = 
  List.filter_map (fun id -> if (List.mem id existing) then (Some (make_event_decl id)) else None) ids
;;

let replacer = 
  object (self) 
    inherit [_] s_map as super

    method! visit_DEvent env id s cs ps= 
      let pes = (!env).prior_events in
      if (not (List.mem id pes)) then env := {(!env) with prior_events = (id :: pes)}; DEvent(id, s, cs, ps)

    method! visit_DAlphabet env id members = 
      match members with 
      | [] -> Console.error @@ "Alphabet should not be empty"
      | _ -> (env := {(!env) with alph_map = (IdMap.add id members (!env).alph_map)}); DAlphabet (id, members)
    
    method! visit_DVarRegex env id z alph vr = 
      let new_alph = (match alph.alph with 
      | AExplicit(_) | AUnspecified -> alph
      | AName (a_id) -> alphabet_explicit (IdMap.find a_id (!env).alph_map) alph.alphabet_span) in
      DVarRegex (id, z, new_alph, vr)
  end

let replace_alphabet env id members = 
  let prior_events = (!env).prior_events in
  let make_decl eid = 
    if (List.mem eid prior_events) then None else Some (decl (DEvent (eid, EBackground, [], []))) in
  List.filter_map make_decl members

let process_prog ds = 
  let env = (ref {prior_events = []; alph_map = IdMap.empty}) in
  let ds = replacer#visit_decls env ds in 
  let replace d = 
    (match d.d with
    | DAlphabet (id, members) -> (replace_alphabet env id members)
    | _ -> [d]) in
  List.flatten (List.map replace ds)

