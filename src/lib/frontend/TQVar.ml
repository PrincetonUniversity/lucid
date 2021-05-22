(* Just wrapping these in a module so we can easily include them in Syntax.ml,
   since ppx_import is acting weird *)
module TQVar_tys = struct
  type level = int

  and id = [%import: Id.t]

  and 'a tyvar =
    | Unbound of id * level
    | Link of 'a

  and 'a tqvar =
    | TVar of 'a tyvar ref
    | QVar of id
  [@@deriving
    visitors
      { name = "tqvar_iter"
      ; variety = "iter"
      ; polymorphic = true
      ; data = true
      ; concrete = true
      }
    , visitors
        { name = "tqvar_map"
        ; variety = "map"
        ; polymorphic = true
        ; data = true
        ; concrete = true
        }]
end

open TQVar_tys

module type TQVarArg = sig
  type a (* A type which includes a tqvar constructor *)

  (* Returns some if a is a tqvar, and None otherwise *)
  val proj : a -> a tqvar option

  (* Function which applies the tqvar constructor *)
  val constr : a tqvar -> a
end

module Make (A : TQVarArg) = struct
  type t = A.a tqvar

  let rec strip_links a =
    match A.proj a with
    | Some (TVar { contents = Link a' }) -> strip_links a'
    | _ -> a
  ;;

  let equiv_tqvar equiv_a t a =
    match t, A.proj (strip_links a) with
    | QVar id1, Some (QVar id2) -> Id.equal id1 id2
    | ( TVar { contents = Unbound (id1, l1) }
      , Some (TVar { contents = Unbound (id2, l2) }) ) ->
      Id.equal id1 id2 && l1 = l2
    | TVar { contents = Link a' }, _ -> equiv_a (strip_links a') a
    | _ -> false
  ;;

  let phys_equiv_tqvar t a =
    match t, A.proj (strip_links a) with
    | QVar id1, Some (QVar id2) -> Id.equal id1 id2
    | TVar r1, Some (TVar r2) -> r1 == r2
    | _ -> false
  ;;
end
